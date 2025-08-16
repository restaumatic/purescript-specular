const puppeteer = require("puppeteer");

(async () => {
  const browser = await puppeteer.launch({ args: ["--no-sandbox"] });
  const page = await browser.newPage();

  const finished = new Promise((resolve) => {
    page.on("console", (msg) => {
      (async () => {
        if (msg.text() === "finished") {
          resolve();
          return;
        }
        var args = [];
        for (var arg of msg.args()) {
          const value = await arg.jsonValue();
          args.push(value);
        }
        console.log.apply(console, args);
      })();
    });
  });
  page.on("pageerror", (err) => {
    console.error(err);
    process.exit(1);
  });

  await page.goto("file://" + process.cwd() + "/bench/benchmark.html");
  await finished;
  await browser.close();
})();
