# HTTP
HTTP Handler for TCP Server designed for Wolfram Kernel (Mathematica, Engine)

## Installation

Via Wolfram Paclet system

```mathematica
PacletInstall["KirillBelov/HTTP"]
```

## Independent request processing

`HTTPHandler` transforms an association containing a **complete HTTP request** in
`"DataByteArray"` into a `ByteArray` containing the HTTP response. It does not read,
write, close sockets, change the transport handler, or assign `"Result"`.
`"SourceSocket"` is optional. Incoming metadata is available to application callbacks
alongside parsed `"Method"`, `"Path"`, `"Query"`, `"Headers"`, `"BodyByteArray"`,
`"Body"`, and `"Content"` fields.

```wolfram
Needs["WLJS`HTTP`"];

http = HTTPHandler[];
http["MessageHandler", "health"] =
    (HTTPRequestMatchQ[#, <|"Method" -> "GET", "Path" -> "/health"|>]& ) ->
    Function[<|"Body" -> "ok", "ContentType" -> "text/plain; charset=utf-8"|>];

packet = <|"DataByteArray" -> StringToByteArray["GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n"]|>;
responseBytes = http[packet];
processedPacket = Append[packet, "Result" -> responseBytes];
```

Callbacks can return a string, `ByteArray`, `HTTPResponse`, or an association with
`"Body"`, `"StatusCode"`, `"ContentType"`, and `"Headers"`. Legacy `"Code"` is also
accepted. The default response is 404. `"Serializer"`/`"DefaultSerializer"` process
response bodies; `"Deserializer"`/`"DefaultDeserializer"` process request content.
Response strings use UTF-8 unless an explicit charset is supplied; `Content-Length`
counts transmitted bytes, including compression when requested.

## Content serialization

`HTTPSerialize[body, contentType: Automatic]` returns
`<|"Body" -> ByteArray[...], "ContentType" -> "..."|>`. The body is a payload, not a
response envelope: an association passed here is JSON data, even if it has a `"Body"` key.
`HTTPDeserialize[bytes, contentType]` returns decoded content. After `Needs["WLJS`HTTP`"]`
both functions can be used without constructing a handler or loading CSockets.
`HTTPDeserialize[request]` reads the parsed request's `"BodyByteArray"` and `"ContentType"`
(or `"Headers"`). Whole empty request bodies decode to `Null`; unknown media types
preserve their bytes. Invalid supported content returns `$Failed` with a message;
HTTPHandler then returns `$Failed` without invoking the application callback.

| Content-Type | Serialization / deserialization |
| --- | --- |
| `application/json`, `application/*+json` | Associations, lists, numbers, booleans, `Null` / JSON data |
| `text/*` (plain text, HTML, CSS, etc.) | Strings with the declared charset (UTF-8 by default) |
| `application/x-www-form-urlencoded` | Association or list of rules / association; repeated keys become lists |
| `multipart/form-data` | Association or ordered list of part descriptors / ordered list of parts |
| `application/xml`, `text/xml`, `*+xml` | `XMLElement` or `XMLObject` / imported XML document |
| `text/csv`, `text/tab-separated-values` | Tables / imported tables |
| PNG, JPEG, GIF, BMP, TIFF, SVG | Wolfram image/graphics values / Wolfram import result |
| `application/octet-stream` and unknown types | `ByteArray` preserved unchanged |
| `application/vnd.wolfram.expression+json` | ExpressionJSON export/import |

Automatic serialization chooses JSON for associations/lists/scalars, PNG for `Image`,
SVG for `Graphics`, XML for XML expressions, and octet-stream for bytes. Strings keep
the existing HTML default. `File[path]` reads the file as bytes and infers its media
type from the extension; unknown extensions use octet-stream. Files and ByteArray
payloads are treated as already encoded. A string with an explicit structured media
type (e.g. JSON or XML) is also treated as already encoded text.

```wolfram
encoded = HTTPSerialize[<|"ok" -> True|>];
data = HTTPDeserialize[encoded["Body"], encoded["ContentType"]];

http["DefaultMessageHandler"] = Function[
    <|"Body" -> <|"ok" -> True|>, "StatusCode" -> 200|>
];
(* Content-Type is automatically application/json; charset=utf-8. *)

http["DefaultMessageHandler"] = Function[
    <|"Body" -> {{"name", "value"}, {"a", "1"}}, "ContentType" -> "text/csv"|>
];
```

The existing per-handler conditional `"Serializer"` and `"Deserializer"` maps take
precedence over the built-ins. Serializer callbacks receive the body and can return
bytes, text, or an association with `"Body"`, `"ContentType"`, and optional metadata.
Deserializer callbacks receive the complete parsed request.

### Multipart forms and uploads

Parts follow RFC 7578: duplicate field names remain separate parts in wire order.
Each decoded part contains `"Name"`, `"Headers"`, `"ContentType"`, `"Body"` (the exact
bytes), and `"Content"` (decoded data). File parts also have `"FileName"`.
Empty text fields decode to `""`; empty binary parts retain an empty ByteArray.
Uploads stay in memory; the library does not write filenames supplied by clients.
Nested legacy `multipart/mixed` content is retained as bytes.

```wolfram
form = HTTPSerialize[{
    <|"Name" -> "tag", "Body" -> "one"|>,
    <|"Name" -> "tag", "Body" -> "two"|>,
    <|"Name" -> "upload", "FileName" -> "data.bin", "Body" -> ByteArray[{0, 255}]|>
}, "multipart/form-data"];
parts = HTTPDeserialize[form["Body"], form["ContentType"]];
```

Serialization generates a boundary and adds it to Content-Type. An explicit boundary
is accepted if valid and absent from the encoded parts. A missing or truncated
boundary on input returns `$Failed`. Quoted header parameters (including semicolons
in filenames) are supported.

### Content-Encoding

HTTPHandler decodes request `Content-Encoding: gzip` before deserialization.
Use `"ContentEncoding" -> "gzip"` or a `"Content-Encoding"` response header to compress
the serialized response; Content-Length is recalculated after compression. Identity
and comma-separated sequences of gzip/identity are supported. Other encodings return
`$Failed`. Raw `HTTPResponse` results are exported as supplied.

```wolfram
http["DefaultMessageHandler"] = Function[
    <|"Body" -> <|"ok" -> True|>, "ContentEncoding" -> "gzip"|>
];
```

## CSockets integration

Configure the transport explicitly (the old `handler += http` shorthand is removed):

```wolfram
Needs["WLJS`CSockets`"];

handler = CSocketHandler[
    "Accumulator" -> HTTPPacketLength,
    "Received" -> http
];
server = CSocketOpen["127.0.0.1", 8080];
listener = SocketListen[server, handler];
```

CSocketHandler assembles the request, invokes `http[packet]`, stores its returned
bytes in `"Result"`, and sends them. Its `"Logger"` observes the packet after sending.
Use the default identity transport serializer so the response bytes pass through.

`HTTPPacketQ` recognizes HTTP request packets. `HTTPPacketLength` computes the first
request's size from the header delimiter and `Content-Length`, excluding subsequent
pipelined requests. With the current CSockets accumulator contract, **the complete
HTTP header must be present in the first fragment**: CSockets calculates expected
length only once. A split body is supported; split headers and chunked transfer
framing need a transport framing strategy before invoking HTTPHandler.

Run the live example with `wolframscript -f Test.wls`, or the static website example
with `wolframscript -f Scripts/StaticWebSite.wls`.

## Source layout

All runtime code lives in `Kernel/HTTP.wl`: HTTP processing, content serialization,
MIME/charset tables and the legacy ``WLJS`HTTP`Extensions` `` helpers. Both package
contexts load from this file. `PacletInfo.wl` remains separate for paclet registration.
The former `Kernel/Serialization.wl` and `Kernel/Extensions.wl` are merged into it.
`AGENTS.md` describes the WL formatting rules and the transport boundary.

## Tests

Run `wolframscript -f Tests/RunTests.wls` for content format tests, independent processing tests and the
CSocketHandler integration test. The HTTP package itself needs Objects, not CSockets;
the integration test loads the sibling CSockets checkout.

## Examples

Clone this repository

```bash
git clone https://github.com/KirillBelovTest/HTTP
cd HTTP
```

There is a set of usefull example, that helps to understand the logic of a webserver and its applications

### APIFunction

This basically replicates the functionalify of Wolfram `APIFunction` available in Cloud only. However, using our approach one can run it locally

```bash
wolframscript -f Examples/APIFunction.wls
```

then open your browser at

```url
http://127.0.0.1:8000/cellular?rule=69&step=500
http://127.0.0.1:8000/bars?v=1,2,3,4,5,6
```

what it does, it provides the URL parameters to the defined API function and generates the result for the reply

![](./imgs/Screenshot%202023-07-26%20at%2011.25.44.png)

![](./imgs/Screenshot%202023-07-26%20at%2011.27.42.png)

### StaticWebsite

This is a simple example of templating using `StringTemplate` to embed the data into an html document from the request parameters or call an arbitary Wolfram Language function 

```bash
wolframscript -f Examples/StaticWebsite.wls
```

![](imgs/Screenshot%202023-07-26%20at%2011.18.50.png)

It also shows how to work with media files and paths for the server. Those two WL logos are requested from two different folders, specified in `Base` parameters as an array of paths. Also the current date and a list with request parameters shows how to embed the WL function into the document.  

### FormExample

A bit more advanced example - a follow up to `APIFunction` - that unleash to power of modern HTML/CSS stack and POST method. It relies on [Wolfram Script Pages](https://github.com/JerryI/wl-wsp) framework for easier templating bringing simillar experiense if you was writting in PHP

```bash
wolframscript -f Examples/FormExample.wls
```

One can design anything in any share, using its favourite JS/CSS framework. Here Bootstrap is used. It has almost no limitations, when it comes to the static content generation compared to Wolfram Cloud

![](imgs/Screenshot%202023-07-26%20at%2011.35.56.png)

Each time you drag a slider and click to `Submit` button, it sends POST request, recalculates a new plot and sends to a user. No JS required, just old good HTTP and reqular forms processing.

### POST

This is a demonstartion of how to work with POST request. It can process only text files and text data unfortonately. When you upload the data and submit it, it redirects to the specified page, while the recived data is printed to the console

```bash
wolframscript -f Examples/POST.wls
```

### WSP

This demo shows on how to work with WSP ([Wolfram Script Pages](https://github.com/JerryI/wl-wsp)) template engine and generate content based on user's request passed via URL parameters

```bash
wolframscript -f Examples/WSP.wls
```

*index.wsp*  

```php
<?wsp With[{color = $CurrentRequest["Query", "color"]}, ?>
    <div style="width:100px; height:100px; background-color: <?wsp color ?>"></div>
<?wsp ] ?>
```

As one can see, it puts a color variable into the style of a `div` element. Then if one open a page at 

```
http://127.0.0.1:8000/?color=cyan
```

it will show

![](imgs/Screenshot%202023-07-26%20at%2011.42.59.png)

### Plotter ⚗️
This features a file explorer & graph viewer made out of independent components simillar to what people do in a modern WEB

```bash
wolframscript -f Examples/Plotter.wls
```

One can navigate through the folders

![](imgs/Screenshot%202023-07-26%20at%2011.46.27.png)

and open the sample data

![](imgs/Screenshot%202023-07-26%20at%2011.46.58.png)

For each file there is a `view.wsp` component that generates SVG image and embeds it into a page.

> Cool, ha? Thinking about what you can do, makes Wolfram Cloud to be just a toy for the rich kids - @JerryI

### Plotter2 🔭
One could go even further utulizing the power of [WebSockets](https://github.com/KirillBelovTest/WebSocketHandler) and [WLJS](https://github.com/JerryI/wljs-interpreter) (a tiny Wolfram Language Interpreter running in a browser). So that we do not need to spend resources on generating ugly static SVGs, but providing just the raw data and make browser and Javascript to do the actual plotting

```bash
wolframscript -f Examples/Plotter2.wls
```

The naviagtion is the same, but when a user opens a file it shows a placeholde, while Javascript is pumping the data from the server via WebSockets (aka hydrate the HTML element) and then plots the received data

![](imgs/ezgif.com-video-to-gif-27.gif)

This is not about rocket science, but a combination of a few simple open-source tools, that moves Wolram Language beyong the notebook interface
