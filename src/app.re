[%bs.raw {|require('../../../src/App.scss')|}];

type state = {
  sides: int,
  friction: float,
  roundToNearestSide: bool,
  controls: bool,
};

type action =
  | Sides(int)
  | Friction(int)
  | RoundToNearestSide(bool)
  | Controls(bool);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    sides: 8,
    friction: 0.0175,
    roundToNearestSide: true,
    controls: false,
  },
  reducer: (action, state) =>
    switch (action) {
    | Sides(n) => ReasonReact.Update({...state, sides: state.sides + n})
    | Friction(slider) =>
      let friction = 0.045 *. float_of_int(slider) /. 100.0 +. 0.005;
      ReasonReact.Update({...state, friction});
    | RoundToNearestSide(roundToNearestSide) =>
      ReasonReact.Update({...state, roundToNearestSide})
    | Controls(show) => ReasonReact.Update({...state, controls: show})
    },
  render: self => {
    let sides = self.ReasonReact.state.sides;
    let friction = self.ReasonReact.state.friction;
    let roundToNearestSide = self.ReasonReact.state.roundToNearestSide;
    let slider = int_of_float((friction -. 0.005) *. 100.0 /. 0.045);
    <div>
      <Container sides friction roundToNearestSide />
      {self.ReasonReact.state.controls
         ? ReasonReact.null
         : <a
             className="trigger"
             onClick={_event => self.ReasonReact.send(Controls(true))}>
             {ReasonReact.string("Controls")}
           </a>}
      <div
        className={
          self.ReasonReact.state.controls ? "controls active" : "controls"
        }>
        <button
          className="close"
          onClick={_event => self.ReasonReact.send(Controls(false))}>
          {ReasonReact.string({js|\u00d7|js})}
        </button>
        <div className="control">
          <button
            onClick={_event => self.ReasonReact.send(Sides(-1))}
            disabled={sides == 3 ? true : false}>
            {ReasonReact.string(" - ")}
          </button>
          {ReasonReact.string("Sides: ")}
          <button onClick={_event => self.ReasonReact.send(Sides(1))}>
            {ReasonReact.string(" + ")}
          </button>
        </div>
        <div className="control">
          {ReasonReact.string("Friction: ")}
          <input
            type_="range"
            value={string_of_int(slider)}
            onChange={event =>
              self.ReasonReact.send(
                Friction(ReactEvent.Form.target(event)##value),
              )
            }
          />
        </div>
        <label className="control">
          {ReasonReact.string("Round to nearest side: ")}
          <input
            type_="checkbox"
            checked={roundToNearestSide ? true : false}
            onChange={event =>
              self.ReasonReact.send(
                RoundToNearestSide(ReactEvent.Form.target(event)##checked),
              )
            }
          />
        </label>
      </div>
      {self.state.controls
         ? ReasonReact.null
         : <a
             className="github"
             href="https://github.com/persianturtle/reason-carousel">
             {ReasonReact.string("Fork me on Github")}
           </a>}
    </div>;
  },
};