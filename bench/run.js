const puppeteer = require("puppeteer");
const express = require("express");

(async () => {
  const app = express();
  app.use(express.static("."));
  app.listen(8992);

  const browser = await puppeteer.launch({ args: ["--no-sandbox"] });
  const page = await browser.newPage();

  let done;
  const donePromise = new Promise((resolve) => (done = resolve));

  page.on("console", async (msg) => {
    var args = [];
    for (var arg of msg.args()) {
      const value = await arg.jsonValue();
      args.push(value);
    }
    console.log.apply(console, args);
    if (msg.text() === "done") {
      done();
    }
  });

  page.on("pageerror", async (msg) => {
    console.log("ERROR: " + msg);
    done();
  });

  await page.goto("http://localhost:8992/bench/benchmark.html");

  await donePromise;
  await browser.close();
  process.exit(1);
})();
