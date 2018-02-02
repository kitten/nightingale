open Reprocessing;
open Animation;

type bodyT = {
  isOnGround: bool,
  mass: float,
  size: (float, float),
  velocity: (float, float),
  force: (float, float),
  restitution: float,
  gravityFactor: float,
  staticFriction: float,
  position: (float, float)
};

let makeBody = (
  ~position: (float, float),
  ~mass: float,
  ~restitution: float,
  ~size: (float, float),
  ~staticFriction: float
) => {
  isOnGround: false,
  mass,
  size,
  velocity: (0., 0.),
  force: (0., 0.),
  restitution,
  gravityFactor: 1.,
  staticFriction,
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

let step = (dt: float, body: bodyT, isOnGround: bool) =>
  if (body.mass == 0.) {
    body
  } else {
    let (vx, vy) = body.velocity;
    let (forceX, forceY) = body.force;

    let dvx = forceX /. body.mass *. dt;
    let dvy = forceY /. body.mass *. dt;

    let new_vx = vx +. dvx;
    let new_vy = vy +. dvy;

    let velocity = (new_vx, new_vy);
    let position = add(body.position, (new_vx *. dt, new_vy *. dt));

    let gravityForceY = 10. *. body.gravityFactor *. body.mass;
    let force = (0., isOnGround ? 0. : gravityForceY);
    {...body, velocity, position, force}
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

let getInvmass = (body: bodyT) =>
  if (body.mass == 0.) { 0. } else { 1. /. body.mass };

let applyFriction = (bodyA: bodyT, bodyB: bodyT, n: (float, float)) => {
  let invmass_a = getInvmass(bodyA);
  let invmass_b = getInvmass(bodyB);
  let rv = subtract(bodyB.velocity, bodyA.velocity); /* relative velocity */

  /* define tangent */
  let t = normalise(subtract(rv, mult(dot(rv, n), n)));
  let ve_t = dot(rv, t); /* relative velocity along tangent */

  /* calculate friction to slow objects down */
  let mu = length((bodyA.staticFriction, bodyB.staticFriction));
  let jt = (-1. *. mu *. ve_t) /. (invmass_a +. invmass_b);
  let frictionImpulse = mult(jt, t);

  /* apply velocities based on the bodies' mass ratio */
  let velocityA = subtract(bodyA.velocity, mult(invmass_a, frictionImpulse));
  let velocityB = add(bodyB.velocity, mult(invmass_b, frictionImpulse));

  (
    { ...bodyA, velocity: velocityA },
    { ...bodyB, velocity: velocityB }
  )
};

let applyImpulse = (bodyA: bodyT, bodyB: bodyT, n: (float, float), depth: float) => {
  let rv = subtract(bodyB.velocity, bodyA.velocity); /* relative velocity */
  let ve_n = dot(rv, n); /* relative velocity along normal */

  let invmass_a = getInvmass(bodyA);
  let invmass_b = getInvmass(bodyB);

  if (ve_n > 0. || abs_float(depth) < driftSlop) {
    /* floating-point drift correction */
    let d = depth /. (invmass_a +. invmass_b);

    /* apply drift correction */
    let correction = mult(d, n);
    let positionA = subtract(bodyA.position, mult(invmass_a, correction));
    let positionB = add(bodyB.position, mult(invmass_b, correction));

    let bodyA = { ...bodyA, position: positionA };
    let bodyB = { ...bodyB, position: positionB };
    applyFriction(bodyA, bodyB, n)
  } else {
    /* calculate impulse to move objects apart */
    let e = min(bodyA.restitution, bodyB.restitution); /* restitution coefficient */
    let j = (-0.9 *. (1. +. e) *. ve_n) /. (invmass_a +. invmass_b); /* impulse scalar */
    let impulse = mult(j, n);

    /* floating-point drift correction */
    let d = max(depth -. driftSlop, 0.) /. (invmass_a +. invmass_b) *. driftCorrection;

    /* apply drift correction */
    let correction = mult(d, n);
    let positionA = subtract(bodyA.position, mult(invmass_a, correction));
    let positionB = add(bodyB.position, mult(invmass_b, correction));

    /* apply reflection velocities */
    let velocityA = subtract(bodyA.velocity, mult(invmass_a, impulse));
    let velocityB = add(bodyB.velocity, mult(invmass_b, impulse));

    let bodyA = { ...bodyA, velocity: velocityA, position: positionA };
    let bodyB = { ...bodyB, velocity: velocityB, position: positionB };
    applyFriction(bodyA, bodyB, n)
  }
};
