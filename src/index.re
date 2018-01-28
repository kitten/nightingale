open Reprocessing;

type stateT('instance) = {
  global: Global.stateT,
  instance: 'instance
};

let entity = Entity.instance(Protagonist.make())
  |> Entity.composeInstances(Entity.instance(Floor.make()));

let setup = (env) => {
  let global = Global.setup(env);
  let instance = entity.setup(global, env);
  { global: global, instance: instance }
};

let draw = ({ global: prevGlobal, instance: prevInstance }, env) => {
  let global = Global.step(prevGlobal, env);
  let instance = entity.step(prevInstance, global, env);
  let global = Global.bodyResolution(global, env);
  Global.draw(global, env);
  entity.draw(instance, global, env);
  { global: global, instance: instance }
};

run(~setup, ~draw, ());
