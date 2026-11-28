// The backdrop of soundness.dev behind a presentation: a lattice of dots, displaced by waves
// from three sources. It is Glimmer's `ripple.js`, with two differences: it makes its own
// canvas, since a presentation's page has none, and a wave travels a wavelength in 20 seconds
// where the website's takes 60. A deck names this file with `script`; `soundness.css` puts the
// canvas behind the slides.
(() => {
  const config = {
    animate: !/[?&]static\b/.test(location.search), // set false (or add ?static) for a still image

    // Grid
    spacing: 25,          // px between neighbouring dots
    rotation: 7.91,       // degrees; at 7.91 no two dots within 150px share a pixel row or column

    // Wave
    wavelength: 1000,     // base wavelength in px (measured horizontally)
    amplitude: 40,        // max displacement in px, above or below a dot's rest position
    decay: 1.0,           // fade with distance: magnitude = amplitude / (1 + decay * wavelengths travelled); 0 = never
    period: 20,           // seconds for a wave to travel one base wavelength (the website's is 60)
    depthScale: Math.sqrt(3), // isometric foreshortening: ground distance per vertical screen px
    staticPhase: 8,       // moment (in base periods) shown when not animating

    // Colours (the page background is set in the stylesheet; the canvas itself is transparent)
    dot: '#ff4400',
    dotSize: 1,           // px
    scrollFade: 0.5,      // dot intensity from the bottom of the first screenful onwards (1 at the page top)
  };

  // ---- Generators --------------------------------------------------------------------------
  // Each generator has a position (as a fraction of the viewport), a gain, and a signal: what
  // it emits as a function of time `u` measured in base periods, returning -1..1. A dot at
  // distance r (in base wavelengths) shows the signal as it was r periods ago, so every
  // frequency travels outwards at the same speed, as sound does. Where waves meet, they add.
  const TAU = 2 * Math.PI, sin = Math.sin;

  // A tone whose pitch slides between f - 0.5 and f + 0.5, `rate` times per base period.
  const warbling = (f, rate, phase) => u => sin(TAU * f * u + 0.5 / rate * sin(TAU * rate * u + phase));

  // Three warbling generators on harmonics 2, 3 and 4 of a shared fundamental, so their
  // wavelengths are 1, 2/3 and 1/2 of the base. Each warbles at its own rate.
  const generators = [
    { x: 0.50, y: 0.30, signal: warbling(1.00, 0.100, 0.0), gain: 0.65 },
    { x: 0.18, y: 0.62, signal: warbling(1.50, 0.135, 2.1), gain: 0.65 },
    { x: 0.84, y: 0.55, signal: warbling(2.00, 0.075, 4.2), gain: 0.65 },
  ];
  // ------------------------------------------------------------------------------------------

  const canvas = document.createElement('canvas');
  canvas.id = 'ripple';
  canvas.setAttribute('aria-hidden', 'true');
  document.body.prepend(canvas);
  const ctx = canvas.getContext('2d');
  const G = generators.length;
  let width = 0, height = 0, count = 0;
  let now = config.animate ? 0 : config.staticPhase;   // current time, in base periods
  let lastMs = null;
  let xs, ys;                   // per-dot snapped position
  let dists, amps;              // per-dot, per-generator: distance in wavelengths, and amplitude

  function build() {
    const dpr = window.devicePixelRatio || 1;
    width = window.innerWidth;
    height = window.innerHeight;
    canvas.width = Math.round(width * dpr);
    canvas.height = Math.round(height * dpr);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

    const s = config.spacing, t = config.rotation * Math.PI / 180;
    const ax = s * Math.cos(t), ay = s * Math.sin(t);
    const bx = s * Math.cos(t + Math.PI / 3), by = s * Math.sin(t + Math.PI / 3);
    const margin = config.amplitude + 2;

    // Enumerate the triangular lattice (anchored a third of the way down the viewport).
    const ox = width / 2, oy = height / 3;
    const reach = Math.hypot(ox, height - oy) + margin;
    const n = Math.ceil(reach / (s * Math.sin(Math.PI / 3))) + 1;
    const X = [], Y = [];
    for (let i = -n; i <= n; i++) {
      for (let j = -n; j <= n; j++) {
        const x = ox + i * ax + j * bx, y = oy + i * ay + j * by;
        if (x < -1 || x > width + 1 || y < -margin || y > height + margin) continue;
        X.push(Math.floor(x) + 0.5);  // snap to pixel columns/rows so dots stay crisp
        Y.push(Math.floor(y) + 0.5);
      }
    }
    count = X.length;
    xs = Float32Array.from(X); ys = Float32Array.from(Y);

    // Each dot's distance from each generator, measured on the ground plane, in wavelengths.
    dists = new Float32Array(count * G); amps = new Float32Array(count * G);
    for (let g = 0; g < G; g++) {
      const gx = generators[g].x * width, gy = generators[g].y * height;
      const gain = config.amplitude * generators[g].gain;
      for (let k = 0; k < count; k++) {
        const r = Math.hypot(X[k] - gx, (Y[k] - gy) * config.depthScale) / config.wavelength;
        dists[k * G + g] = r;
        amps[k * G + g] = gain / (1 + config.decay * r);
      }
    }
  }

  function draw() {
    const d = config.dotSize;
    ctx.clearRect(0, 0, width, height);

    // One dot per grid point, displaced vertically by the sum of the waves reaching it.
    ctx.fillStyle = config.dot;
    ctx.beginPath();
    for (let k = 0, i = 0; k < count; k++) {
      let h = 0;
      for (let g = 0; g < G; g++, i++) h += amps[i] * generators[g].signal(now - dists[i]);
      ctx.rect(xs[k] - d / 2, ys[k] - h - d / 2, d, d);
    }
    ctx.fill();

    // Scroll fade: thin the dots out, from full strength at the top of the page to scrollFade
    // at the bottom of the first screenful, and constant below that.
    const top = -window.scrollY;
    const fade = ctx.createLinearGradient(0, top, 0, top + height);
    fade.addColorStop(0, 'rgba(0, 0, 0, 1)');
    fade.addColorStop(1, `rgba(0, 0, 0, ${config.scrollFade})`);
    ctx.globalCompositeOperation = 'destination-in';
    ctx.fillStyle = fade;
    ctx.fillRect(0, 0, width, height);
    ctx.globalCompositeOperation = 'source-over';
  }

  function frame(ms) {
    if (lastMs !== null && config.animate) now += Math.min(ms - lastMs, 100) / 1000 / config.period;
    lastMs = ms;
    draw();
    if (config.animate) requestAnimationFrame(frame);
  }

  if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
    config.animate = false;
    now = config.staticPhase;
  }
  window.addEventListener('resize', () => { build(); if (!config.animate) draw(); });
  window.addEventListener('scroll', () => { if (!config.animate) draw(); }, { passive: true });
  build();
  requestAnimationFrame(frame);
})();
