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
