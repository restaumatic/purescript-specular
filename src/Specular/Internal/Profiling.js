if (typeof global === "undefined") { global = window; };

// global.SPECULAR_PROFILING_ENABLED = true;

const enabled = !!global.SPECULAR_PROFILING_ENABLED;

const frameNameToIndex = {};
const frames = [];
const events = [];

global.SpecularProfiling = {
  open: (url) => {
    const w = window.open(url || 'http://localhost:1234');
    window.addEventListener('message', (event) => {
      if(event.source === w && event.data.type === 'getProfile') {
        console.log('received getProfile');
        w.postMessage({ type: 'loadInitialProfile', profile: SpecularProfiling.getProfile() }, '*');
      }
    });
  },

  getProfile() {
    return {
      shared: {
        frames: frames
      },
      profiles: [
        {
          type: 'evented',
          name: 'page',
          unit: 'milliseconds',
          startValue: events.length !== 0 ? events[0].at : 0,
          endValue: events.length !== 0 ? events[events.length - 1].at : 0,
          events: events
        }
      ]
    };
  },

  clear: () => {
    events.length = 0;
  },
};

function begin(name) {
  let frameIndex = frameNameToIndex[name];
  if(frameIndex === undefined) {
    frameIndex = frames.length;
    frameNameToIndex[name] = frameIndex;
    frames.push({ name: name });
  }
  events.push({ type: 'O', frame: frameIndex, at: performance.now() });
  return frameIndex;
}

function end(frame) {
  events.push({ type: 'C', frame: frame, at: performance.now() });
}

// enabled :: Boolean
exports.enabled = enabled;

exports.none = 0;

// begin :: EffectFn1 String Unit
exports.begin = enabled ? begin : () => {};

// end :: EffectFn1 String Unit
exports.end = enabled ? end : () => {};
