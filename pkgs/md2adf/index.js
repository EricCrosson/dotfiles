#!/usr/bin/env node
const { MarkdownTransformer } = require("@atlaskit/editor-markdown-transformer");
const { JSONTransformer } = require("@atlaskit/editor-json-transformer");
const { defaultSchema } = require("@atlaskit/adf-schema/schema-default");

const md = new MarkdownTransformer(defaultSchema);
const json = new JSONTransformer();

let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => (input += chunk));
process.stdin.on("end", () => {
  process.stdout.write(JSON.stringify(json.encode(md.parse(input))));
});
