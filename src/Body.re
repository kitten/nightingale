open Reprocessing;
open Animation;

type bodyT = {
  hasCollision: bool,
  mass: float,
  size: (float, float),
  velocity: (float, float),
  force: (float, float),
  restitution: float,
  gravityFactor: float,
  staticFriction: float,
  dynamicFriction: float,
  position: (float, float)
};

let makeBody = (
  ~position: (float, float),
  ~mass: float,
  ~restitution: float,
  ~size: (float, float),
  ~staticFriction: float,
  ~dynamicFriction: float
) => {
  hasCollision: false,
  mass,
  size,
  velocity: (0., 0.),
  force: (0., 0.),
  restitution,
  gravityFactor: 1.,
  staticFriction,
  dynamicFriction,
  position
};

/* constant determining how many pixels are in a metre */
let px_in_metres = 48.;
/* constant determining the amount of floating-point drift correction */
let driftCorrection = 0.2;
let driftSlop = 0.01;

let getPixelPosition = (body: bodyT) => mult(px_in_metres, body.position);

let getPixelSize = (body: bodyT) => {
  let (w, h) = body.size;
  (int_of_float(w *. px_in_metres), int_of_float(h *. px_in_metres))
};

let stepVelocity = (env: glEnvT, body: bodyT) => {
  let dt = Env.deltaTime(env);
  let (vx, vy) = body.velocity;
  let (dx, dy) = (vx *. dt, vy *. dt);
  let (x, y) = body.position;
  let position = (dx +. x, dy +. y);
  { ...body, position }
};

let applyForce = (env: glEnvT, body: bodyT) =>
  if (body.mass == 0.) {
    body
  } else {
    let dt = Env.deltaTime(env);
    let (forceX, forceY) = body.force;
    let (pvx, pvy) = body.velocity;
    let (ax, ay) = (forceX /. body.mass *. dt, forceY /. body.mass *. dt);
    let velocity = (ax +. pvx, ay +. pvy);
    { ...body, force: (0., 0.), velocity }
  };

let applyGravity = (env: glEnvT, body: bodyT) => {
  let dt = Env.deltaTime(env);
  let { gravityFactor } = body;
  let (pvx, pvy) = body.velocity;
  let velocity = (pvx, 9.8 *. gravityFactor *. dt +. pvy);
  { ...body, velocity }
};

let getCoordinates = (body: bodyT) : ((float, float), (float, float)) => {
  let (minx, miny) = body.position;
  let (sizex, sizey) = body.size;
  let max = (minx +. sizex, miny +. sizey);
  (body.position, max)
};

let isColliding = (bodyA: bodyT, bodyB: bodyT) : bool => {
  let ((a_minx, a_miny), (a_maxx, a_maxy)) = getCoordinates(bodyA);
  let ((b_minx, b_miny), (b_maxx, b_maxy)) = getCoordinates(bodyB);

  !(
    (a_maxx < b_minx || a_minx > b_maxx) ||
    (a_maxy < b_miny || a_miny > b_maxy)
  ) && (bodyA.mass != 0. || bodyB.mass != 0.)
};

/* returns directional collision-normal vector and depth of overlap */
let collisionNormal = (bodyA: bodyT, bodyB: bodyT) : ((float, float), float) =>
  if (!isColliding(bodyA, bodyB)) {
    ((0., 0.), 0.)
  } else {
    let ((a_minx, a_miny), (a_maxx, a_maxy)) = getCoordinates(bodyA);
    let ((b_minx, b_miny), (b_maxx, b_maxy)) = getCoordinates(bodyB);
    let (nx, ny) = subtract(bodyB.position, bodyA.position);

    let extent_ax = a_maxx -. a_minx;
    let extent_bx = b_maxx -. b_minx;
    let extent_tx = (nx < 0. ? extent_bx : extent_ax) -. abs_float(nx);
    let overlap_x = abs_float(-1.0 *. (extent_tx -. extent_bx -. extent_ax) -. extent_bx -. extent_ax);

    let extent_ay = a_maxy -. a_miny;
    let extent_by = b_maxy -. b_miny;
    let extent_ty = (ny < 0. ? extent_by : extent_ay) -. abs_float(ny);
    let overlap_y = abs_float(-1.0 *. (extent_ty -. extent_by -. extent_ay) -. extent_by -. extent_ay);

    if (overlap_x < overlap_y) {
      if (nx < 0.) {
        ((-1., 0.), overlap_x)
      } else {
        ((1., 0.), overlap_x)
      }
    } else {
      if (ny < 0.) {
        ((0., -1.), overlap_y)
      } else {
        ((0., 1.), overlap_y)
      }
    }
  };

let applyImpulse = (bodyA: bodyT, bodyB: bodyT, n: (float, float), depth: float) => {
  let rv = subtract(bodyB.velocity, bodyA.velocity); /* relative velocity */
  let ve_n = dot(rv, n); /* relative velocity along normal */

  if (ve_n > 0.) {
    (bodyA, bodyB)
  } else {
    let invmass_a = if (bodyA.mass == 0.) { 0. } else { 1. /. bodyA.mass };
    let invmass_b = if (bodyB.mass == 0.) { 0. } else { 1. /. bodyB.mass };

    /* calculate impulse to move objects apart */
    let e = min(bodyA.restitution, bodyB.restitution); /* restitution coefficient */
    let j = (-1. *. (1. +. e) *. ve_n) /. (invmass_a +. invmass_b); /* impulse scalar */
    let impulse = mult(j, n);

    /* apply floating-point drift correction */
    let correction = max(depth -. driftSlop, 0.) /. (invmass_a +. invmass_b) *. driftCorrection;
    let positionA = subtract(bodyA.position, mult(correction *. invmass_a, n));
    let positionB = add(bodyB.position, mult(correction *. invmass_b, n));

    /* apply velocities based on the bodies' mass ratio */
    let velocityA = subtract(bodyA.velocity, mult(invmass_a, impulse));
    let velocityB = add(bodyB.velocity, mult(invmass_b, impulse));

    /* redeclare relative velocity due to modification */
    let rv = subtract(velocityB, velocityA); /* relative velocity */

    /* define tangent */
    let t = normalise(subtract(rv, mult(dot(rv, n), n)));
    let ve_t = dot(rv, t); /* relative velocity along tangent */

    /* calculate friction to slow objects down */
    let jt = (-1. *. ve_t) /. (invmass_a +. invmass_b);

    let mu = length((bodyA.staticFriction, bodyB.staticFriction));
    let frictionImpulse = if (abs_float(jt) < j *. mu) {
      mult(jt, t)
    } else {
      let dynamicFriction = length((bodyA.dynamicFriction, bodyB.dynamicFriction));
      mult(-1. *. dynamicFriction *. j, t)
    };

    /* apply velocities based on the bodies' mass ratio */
    let velocityA = subtract(velocityA, mult(invmass_a, frictionImpulse));
    let velocityB = add(velocityB, mult(invmass_b, frictionImpulse));

    (
      { ...bodyA, velocity: velocityA, position: positionA },
      { ...bodyB, velocity: velocityB, position: positionB }
    )
  }
};
