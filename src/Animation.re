let calcKeyframe = (interval: float, numFrames: int, startTick: float, tick: float) : int => {
  let keytime = tick -. startTick;
  let length = interval *. float_of_int(numFrames);
  let chunk = length /. float_of_int(numFrames);
  let keyframe = mod_float(keytime, length) /. chunk;
  int_of_float(keyframe)
};

let pi = 3.14159265358979312;

let isOvershooting = (base, add) => (base < 0.) !== ((base +. add) < 0.);

let min_vec = ((ax, ay), (bx, by)) : (float, float) => {
  let x = ax > bx ? bx : ax;
  let y = ay > by ? by : ay;
  (x, y)
};

let max_vec = ((ax, ay), (bx, by)) : (float, float) => {
  let x = ax > bx ? ax : bx;
  let y = ay > by ? ay : by;
  (x, y)
};

let negate = ((x, y)) : (float, float) => (-1. *. x, -1. *. y);

let clamp = (a, lim) : (float, float) => max_vec(min_vec(a, lim), negate(lim));

let subtract = ((ax, ay), (bx, by)) : (float, float) =>
  (ax -. bx, ay -. by);

let add = ((ax, ay), (bx, by)) : (float, float) =>
  (ax +. bx, ay +. by);

let mult = (n, (x, y)) : (float, float) =>
  (n *. x, n *. y);

let length = ((x, y)) : float =>
  sqrt(x *. x +. y *. y);

let dot = ((ax, ay), (bx, by)) : float =>
  ax *. bx +. ay *. by;

let normalise = (vector) : (float, float) => {
  let extent = length(vector);
  if (extent == 0.) {
    (0., 0.)
  } else {
    let (x, y) = vector;
    (x /. extent, y /. extent)
  }
};

let centreOfPoints = ((ax, ay), (bx, by)) : (float, float) => {
  let (half_x, half_y) = ((bx -. ax) /. 2., (by -. ay) /. 2.);
  (ax +. half_x, ay +. half_y)
};
