open Reprocessing;

type entityT('stateT, 'assetsT, 'actionT) = {
  name: string,
  initState: (glEnvT, Global.stateT) => 'stateT,
  initBody: (glEnvT, Global.stateT) => Body.bodyT,
  initAssets: (glEnvT) => 'assetsT,
  action: ('stateT, Body.bodyT, glEnvT) => 'actionT,
  updateBody: (Body.bodyT, 'actionT, glEnvT, Global.stateT) => Body.bodyT,
  update: ('stateT, Body.bodyT, 'actionT, Global.stateT) => 'stateT,
  render: ('stateT, Body.bodyT, 'assetsT, glEnvT) => unit
};

let initBodyDefault = (_env, _global) : Body.bodyT => {
  isOnGround: false,
  mass: 0.,
  size: (0., 0.),
  velocity: (0., 0.),
  force: (0., 0.),
  restitution: 0.1,
  gravityFactor: 1.,
  staticFriction: 0.5,
  dynamicFriction: 0.2,
  position: (0., 0.)
};

let updateBodyDefault = (body, _action, _env, _global) => body;
let updateDefault = (state, _body, _action, _global) => state;
let renderDefault = (_state, _body, _assets, _env) => ();

let makeEntity = (
  ~name: string,
  ~initState: (glEnvT, Global.stateT) => 'stateT,
  ~initAssets: (glEnvT) => 'assetsT,
  ~action: ('stateT, Body.bodyT, glEnvT) => 'actionT
) => {
  name,
  initState,
  initBody: initBodyDefault,
  initAssets,
  action,
  updateBody: updateBodyDefault,
  update: updateDefault,
  render: renderDefault
};

type entityStateT('stateT, 'assetsT) = {
  state: 'stateT,
  assets: 'assetsT,
  bodyId: int
};

type entityInstanceT('stateT) = {
  setup: (Global.stateT, glEnvT) => 'stateT,
  step: ('stateT, Global.stateT, glEnvT) => 'stateT,
  draw: ('stateT, Global.stateT, glEnvT) => unit
};

let instance = (entity: entityT('stateT, 'assetsT, 'actionT))
  : entityInstanceT(entityStateT('stateT, 'assetsT)) => {
  let setup = (global, env) => {
    let state = entity.initState(env, global);
    let body = entity.initBody(env, global);
    let bodyId = Global.addBody(body, global);
    let assets = entity.initAssets(env);
    { state, assets, bodyId }
  };

  let step = ({ state: prevState, bodyId, assets }, global, env) => {
    let prevBody = Global.getBody(bodyId, global);
    let action = entity.action(prevState, prevBody, env);
    let body = entity.updateBody(prevBody, action, env, global);
    Global.updateBody(global, bodyId, body);
    let state = entity.update(prevState, body, action, global);
    { state, assets, bodyId }
  };

  let draw = ({ state, bodyId, assets }, global, env) => {
    let body = Global.getBody(bodyId, global);
    entity.render(state, body, assets, env);
  };

  { setup: setup, step: step, draw: draw }
};

let composeInstances = (a: entityInstanceT('stateA), b: entityInstanceT('stateB))
  : entityInstanceT(('stateA, 'stateB)) => {
  let setup = (global, env) => {
    let stateA = a.setup(global, env);
    let stateB = b.setup(global, env);
    (stateA, stateB)
  };

  let step = ((prevStateA, prevStateB), global, env) => {
    let stateA = a.step(prevStateA, global, env);
    let stateB = b.step(prevStateB, global, env);
    (stateA, stateB)
  };

  let draw = ((prevStateA, prevStateB), global, env) => {
    a.draw(prevStateA, global, env);
    b.draw(prevStateB, global, env);
  };

  { setup: setup, step: step, draw: draw }
};
