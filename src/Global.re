open Reprocessing;

type stateT = {
  tick: float,
  mutable bodies: array(Body.bodyT)
};

let setup = (env) => {
  Env.size(~width=800, ~height=600, env);
  { tick: 0., bodies: [||] }
};

let step = (state, env) => {
  let ms = Env.deltaTime(env) *. 1000.; /* ms since last frame */
  { ...state, tick: state.tick +. ms }
};

let draw = (_state, env) => {
  Draw.background(Utils.color(~r=47, ~g=54, ~b=64, ~a=255), env);
  Draw.translate(~x=400., ~y=300., env);
};

let addBody = (body: Body.bodyT, global: stateT) : int => {
  let id = Array.length(global.bodies);
  global.bodies = Array.append(global.bodies, [|body|]);
  id
};

let getBody = (bodyId: int, global: stateT) =>
  Array.get(global.bodies, bodyId);

let updateBody = (global: stateT, bodyId: int, body: Body.bodyT) =>
  Array.set(global.bodies, bodyId, body);

let bodyResolution = (state, env) => {
  let bodies = Array.copy(state.bodies);
  let collisionMarker = Array.make(Array.length(bodies), false);
  let visitMarker = Array.make(Array.length(bodies), false);
  let lastIndex = Array.length(bodies) - 1;

  for (i in 0 to lastIndex) {
    Array.set(visitMarker, i, true);

    for (j in 0 to lastIndex) {
      if (!Array.get(visitMarker, j)) {
        Array.set(visitMarker, j, true);
        let bodyA = Array.get(bodies, i);
        let bodyB = Array.get(bodies, j);

        if (Body.isColliding(bodyA, bodyB)) {
          Array.set(collisionMarker, i, true);
          Array.set(collisionMarker, j, true);

          let (newBodyA, newBodyB) = Body.resolveCollision(bodyA, bodyB, env);
          Array.set(bodies, i, newBodyA);
          Array.set(bodies, j, newBodyB);
        };
      };
    };
  };

  for (i in 0 to lastIndex) {
    let hasCollision = Array.get(collisionMarker, i);
    {...Array.get(bodies, i), hasCollision }
      |> Body.applyForce(env)
      |> Body.applyGravity(env)
      |> Body.stepVelocity(env)
      |> Array.set(bodies, i);
  };

  state.bodies = bodies;
  state
};
