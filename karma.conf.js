// See https://medium.com/@rogeriopvl/automating-karma-and-headless-chrome-with-puppetteer-51ce8f6a78b0
process.env.CHROMIUM_BIN = require('puppeteer').executablePath();

module.exports = function(config) {
  config.set({
    browsers: ['ChromiumNoSandbox'],
    singleRun: true,
    customLaunchers: {
      ChromiumNoSandbox: {
        base: "ChromiumHeadless",
        flags: [ "--no-sandbox" ]
      }
    },
    files: [
        "output/BrowserMain.js",
    ],
    frameworks: [
        "mocha",
    ],
    plugins: [
        "karma-chrome-launcher",
        "karma-spec-reporter",
        "karma-mocha",
    ],
    reporters: ["spec"],
  })
};
