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
  Draw.background(Utils.color(~r=44, ~g=52, ~b=61, ~a=255), env);
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

let makePairIdentifier = (i: int, j: int) =>
  i < j ? (i, j) : (j, i);

let createPairs = (bodies: array(Body.bodyT)) => {
  let lastIndex = Array.length(bodies) - 1;
  let table = Hashtbl.create(Array.length(bodies));

  /* check each pair of bodies for collisions and override duplicates with
     collisions of a higher depth */
  for (i in 0 to lastIndex) {
    for (j in 0 to lastIndex) {
      if (i !== j) {
        let bodyA = Array.get(bodies, i);
        let bodyB = Array.get(bodies, j);

        if (bodyA.mass != 0. || bodyB.mass != 0.) {
          let identifier = makePairIdentifier(i, j);
          let norm = Body.collisionNormal(bodyA, bodyB);
          let (_, overlapNew) = norm;

          switch (Hashtbl.find(table, identifier)) {
          | exception Not_found =>
            if (overlapNew != 0.) {
              Hashtbl.add(table, identifier, norm)
            }
          | (_, overlapOld) when overlapNew > overlapOld && overlapNew != 0. =>
            Hashtbl.replace(table, identifier, norm)
          | _ => ()
          };
        };
      };
    };
  };

  /* create pairs from hashtable */
  let pairs = Hashtbl.fold(((i, j), collisionNormal, acc) => {
    let bodyA = Array.get(bodies, i);
    let bodyB = Array.get(bodies, j);
    let pair = (bodyA, bodyB, collisionNormal, (i, j));
    Array.append(acc, [|pair|])
  }, table, [||]);

  /* sort by significance to avoid overriding more important with
     less important collisions */
  Array.stable_sort((
    lhPair: (Body.bodyT, Body.bodyT, ((float, float), float), (int, int)),
    rhPair: (Body.bodyT, Body.bodyT, ((float, float), float), (int, int))
  ) => {
    let (alh, blh, (_, lhDepth), _) = lhPair;
    let (arh, brh, (_, rhDepth), _) = rhPair;
    let (alh_x, alh_y) = alh.position;
    let (arh_x, arh_y) = arh.position;
    let (blh_x, blh_y) = blh.position;
    let (brh_x, brh_y) = brh.position;

    /* higher depth comes later (override),
       higher position (LTR) comes later */
    if (lhDepth < rhDepth) {
      -1
    } else if (alh_x < arh_x && alh_y < arh_y) {
      -1
    } else if (blh_x < brh_x && blh_y < brh_y) {
      -1
    } else {
      1
    }
  }, pairs);

  pairs
};

let bodyResolution = (state, dt) => {
  let bodies = Array.copy(state.bodies);
  let pairs = createPairs(bodies);
  let groundCollisions = Array.make(Array.length(bodies), false);

  /* resolve all pairs' collisions and mark collision */
  Array.iter(pair => {
    let (bodyA, bodyB, (n, depth), (i, j)) = pair;
    let (newBodyA, newBodyB) = Body.applyImpulse(bodyA, bodyB, n, depth);
    Array.set(bodies, i, newBodyA);
    Array.set(bodies, j, newBodyB);

    let (_, ny) = n;
    if (ny > 0.) {
      Array.set(groundCollisions, i, true);
    } else if (ny < 0.) {
      Array.set(groundCollisions, j, true);
    };
  }, pairs);

  /* update the physics loop */
  let bodies = Array.mapi((i: int, body: Body.bodyT) : Body.bodyT => {
    let isOnGround = Array.get(groundCollisions, i);
    {...Body.step(dt, body), isOnGround}
  }, bodies);

  {...state, bodies}
};
