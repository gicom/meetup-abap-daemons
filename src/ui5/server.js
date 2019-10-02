const express = require("express");
const path = require("path");

const PORT = 8081;

express()
    .use("/", express.static(path.join(__dirname, "webapp")))
    .listen(PORT, () => {
        console.log("HTTP server listening on localhost:" + PORT);
    });