const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch({ args: ['--no-sandbox'] });
  const page = await browser.newPage();

  page.on('console', msg => {
    (async () => {
      var args = [];
      for(var arg of msg.args()) {
        const value = await arg.jsonValue();
        args.push(value);
      }
      console.log.apply(console, args);
    })();
  });

  await page.goto('file://' + process.cwd() + '/bench/benchmark.html');
})();
