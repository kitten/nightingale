open Reprocessing;

let calcRunningKeyframe = Animation.calcKeyframe(100., 8);

type animationT =
  | StandingRight
  | StandingLeft
  | RunningRight(int)
  | RunningLeft(int)
  | MidAirRight
  | MidAirLeft;

type stateT = {
  animationStart: float,
  animation: animationT
};

type assetsT = {
  standing: imageT,
  flipStanding: imageT,
  leftStrideJump: imageT,
  leftStrideFalldown: imageT,
  leftStrideContact: imageT,
  leftStrideRecover: imageT,
  rightStrideJump: imageT,
  rightStrideFalldown: imageT,
  rightStrideContact: imageT,
  rightStrideRecover: imageT,
  flipLeftStrideJump: imageT,
  flipLeftStrideFalldown: imageT,
  flipLeftStrideContact: imageT,
  flipLeftStrideRecover: imageT,
  flipRightStrideJump: imageT,
  flipRightStrideFalldown: imageT,
  flipRightStrideContact: imageT,
  flipRightStrideRecover: imageT
};

type actionT =
  | MoveRight
  | MoveLeft
  | JumpRight
  | JumpLeft
  | JumpUp
  | None;

let initState = (_env, _global) => {
  animationStart: 0.,
  animation: StandingRight
};

let initAssets = (env) => {
  standing: Draw.loadImage(~filename="assets/protagonist/standing.png", ~isPixel=true, env),
  flipStanding: Draw.loadImage(~filename="assets/protagonist/flip-standing.png", ~isPixel=true, env),
  leftStrideJump: Draw.loadImage(~filename="assets/protagonist/left-stride-1-jump.png", ~isPixel=true, env),
  leftStrideFalldown: Draw.loadImage(~filename="assets/protagonist/left-stride-2-falldown.png", ~isPixel=true, env),
  leftStrideContact: Draw.loadImage(~filename="assets/protagonist/left-stride-3-contact.png", ~isPixel=true, env),
  leftStrideRecover: Draw.loadImage(~filename="assets/protagonist/left-stride-4-recover.png", ~isPixel=true, env),
  rightStrideJump: Draw.loadImage(~filename="assets/protagonist/right-stride-1-jump.png", ~isPixel=true, env),
  rightStrideFalldown: Draw.loadImage(~filename="assets/protagonist/right-stride-2-falldown.png", ~isPixel=true, env),
  rightStrideContact: Draw.loadImage(~filename="assets/protagonist/right-stride-3-contact.png", ~isPixel=true, env),
  rightStrideRecover: Draw.loadImage(~filename="assets/protagonist/right-stride-4-recover.png", ~isPixel=true, env),
  flipLeftStrideJump: Draw.loadImage(~filename="assets/protagonist/flip-left-stride-1-jump.png", ~isPixel=true, env),
  flipLeftStrideFalldown: Draw.loadImage(~filename="assets/protagonist/flip-left-stride-2-falldown.png", ~isPixel=true, env),
  flipLeftStrideContact: Draw.loadImage(~filename="assets/protagonist/flip-left-stride-3-contact.png", ~isPixel=true, env),
  flipLeftStrideRecover: Draw.loadImage(~filename="assets/protagonist/flip-left-stride-4-recover.png", ~isPixel=true, env),
  flipRightStrideJump: Draw.loadImage(~filename="assets/protagonist/flip-right-stride-1-jump.png", ~isPixel=true, env),
  flipRightStrideFalldown: Draw.loadImage(~filename="assets/protagonist/flip-right-stride-2-falldown.png", ~isPixel=true, env),
  flipRightStrideContact: Draw.loadImage(~filename="assets/protagonist/flip-right-stride-3-contact.png", ~isPixel=true, env),
  flipRightStrideRecover: Draw.loadImage(~filename="assets/protagonist/flip-right-stride-4-recover.png", ~isPixel=true, env)
};

let action = (_state, body: Body.bodyT, env) => {
  let isLeft = Env.key(Events.Left, env) || Env.key(Events.A, env);
  let isRight = Env.key(Events.Right, env) || Env.key(Events.D, env);
  let isUp = Env.key(Events.Up, env) || Env.key(Events.W, env) || Env.key(Events.Space, env);

  switch (body.hasCollision, isLeft, isRight, isUp) {
  | (true, false, true, false) => MoveRight
  | (true, true, false, false) => MoveLeft
  | (true, false, true, true) => JumpRight
  | (true, true, false, true) => JumpLeft
  | (true, false, false, true) => JumpUp
  | _ => None
  }
};

let entity = Entity.makeEntity(~name="Protagonist", ~initState, ~initAssets, ~action);

let make = () : Entity.entityT(stateT, assetsT, actionT) => {
  ...entity,
  initBody: (_env, _global) => {
    let body = Body.makeBody(
      ~position=(0., -2.),
      ~mass=50.,
      ~restitution=0.2,
      ~size=(0.792, 1.83),
      ~staticFriction=0.05,
      ~dynamicFriction=0.35
    );
    { ...body, gravityFactor: 1. }
  },
  updateBody: (body, action, _env, _global) => {
    if (body.hasCollision) {
      let (forceX, forceY) = body.force;

      let newForceX = switch (action) {
      | JumpRight
      | MoveRight when forceX < 500. => 500.
      | JumpLeft
      | MoveLeft when forceX > -500. => -500.
      | _ => forceX
      };

      let newForceY = switch (action) {
      | JumpRight
      | JumpLeft
      | JumpUp when forceY > -7000. => -7000.
      | _ => forceY
      };

      { ...body, force: (newForceX, newForceY) }
    } else {
      body
    }
  },
  update: (state, body, action, { tick }) => {
    switch (state.animation, action) {
    | (_, JumpRight) => {
      ...state,
      animation: MidAirRight
    }
    | (_, JumpLeft) => {
      ...state,
      animation: MidAirLeft
    }
    | (StandingRight | RunningRight(_), JumpUp) => {
      ...state,
      animation: MidAirRight
    }
    | (StandingLeft | RunningLeft(_), JumpUp) => {
      ...state,
      animation: MidAirLeft
    }
    | (StandingRight | StandingLeft | MidAirLeft | MidAirRight, MoveRight) => {
      animationStart: tick,
      animation: RunningRight(calcRunningKeyframe(tick, tick))
    }
    | (StandingRight | StandingLeft | MidAirLeft | MidAirRight, MoveLeft) => {
      animationStart: tick,
      animation: RunningLeft(calcRunningKeyframe(tick, tick))
    }
    | (RunningRight(_) | RunningLeft(_), MoveRight) => {
      ...state,
      animation: RunningRight(calcRunningKeyframe(state.animationStart, tick))
    }
    | (RunningRight(_) | RunningLeft(_), MoveLeft) => {
      ...state,
      animation: RunningLeft(calcRunningKeyframe(state.animationStart, tick))
    }
    | (MidAirRight, None) when body.hasCollision => {
      ...state,
      animation: StandingRight
    }
    | (MidAirLeft, None) when body.hasCollision => {
      ...state,
      animation: StandingLeft
    }
    | (RunningRight(_), None) => {
      ...state,
      animation: StandingRight
    }
    | (RunningLeft(_), None) => {
      ...state,
      animation: StandingLeft
    }
    | _ => state
    }
  },
  render: ({ animation }, body, assets, env) => {
    let (x, y) = Body.getPixelPosition(body);

    let currImage = switch (animation) {
    | RunningRight(0) => assets.leftStrideJump
    | RunningRight(1) => assets.leftStrideFalldown
    | RunningRight(2) => assets.leftStrideContact
    | RunningRight(3) => assets.leftStrideRecover
    | RunningRight(4) => assets.rightStrideJump
    | RunningRight(5) => assets.rightStrideFalldown
    | RunningRight(6) => assets.rightStrideContact
    | RunningRight(_) => assets.rightStrideRecover
    | RunningLeft(0) => assets.flipRightStrideJump
    | RunningLeft(1) => assets.flipRightStrideFalldown
    | RunningLeft(2) => assets.flipRightStrideContact
    | RunningLeft(3) => assets.flipRightStrideRecover
    | RunningLeft(4) => assets.flipLeftStrideJump
    | RunningLeft(5) => assets.flipLeftStrideFalldown
    | RunningLeft(6) => assets.flipLeftStrideContact
    | RunningLeft(_) => assets.flipLeftStrideRecover
    | MidAirRight => assets.leftStrideFalldown
    | MidAirLeft => assets.flipRightStrideFalldown
    | StandingRight => assets.standing
    | StandingLeft => assets.flipStanding
    };

    Draw.pushMatrix(env);
    Draw.translate(~x, ~y=y +. 4., env);
    Draw.image(currImage, ~pos=(-5, -4), ~width=48, ~height=96, env);
    Draw.popMatrix(env);
  }
};
