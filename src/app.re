[%bs.raw {|require('./app.css')|}];

type state = {
  sides: int,
  friction: float,
  roundToNearestSide: bool,
  controls: bool
};

type action =
  | Sides(int)
  | Friction(int)
  | RoundToNearestSide(bool)
  | Controls(bool);

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,
  initialState: () => {sides: 8, friction: 0.0175, roundToNearestSide: true, controls: false},
  reducer: (action, state) =>
    switch action {
    | Sides(n) => ReasonReact.Update({...state, sides: state.sides + n})
    | Friction(slider) =>
      let friction = 0.045 *. float_of_int(slider) /. 100.0 +. 0.005;
      ReasonReact.Update({...state, friction})
    | RoundToNearestSide(roundToNearestSide) => ReasonReact.Update({...state, roundToNearestSide})
    | Controls(show) => ReasonReact.Update({...state, controls: show})
    },
  render: (self) => {
    let sides = self.state.sides;
    let friction = self.state.friction;
    let roundToNearestSide = self.state.roundToNearestSide;
    let slider = int_of_float((friction -. 0.005) *. 100.0 /. 0.045);
    <div>
      <Container sides friction roundToNearestSide />
      (
        self.state.controls ?
          ReasonReact.nullElement :
          <a className="trigger" onClick=(self.reduce((_event) => Controls(true)))>
            (ReasonReact.stringToElement("Controls"))
          </a>
      )
      <div className=(self.state.controls ? "controls active" : "controls")>
        <button className="close" onClick=(self.reduce((_event) => Controls(false)))>
          (ReasonReact.stringToElement({js|\u00d7|js}))
        </button>
        <div className="control">
          <button
            onClick=(self.reduce((_event) => Sides((-1))))
            disabled=(sides == 3 ? Js.true_ : Js.false_)>
            (ReasonReact.stringToElement(" - "))
          </button>
          (ReasonReact.stringToElement("Sides: "))
          <button onClick=(self.reduce((_event) => Sides(1)))>
            (ReasonReact.stringToElement(" + "))
          </button>
        </div>
        <div className="control">
          (ReasonReact.stringToElement("Friction: "))
          <input
            _type="range"
            value=(string_of_int(slider))
            onChange=(
              self.reduce(
                (event) =>
                  Friction(ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value)
              )
            )
          />
        </div>
        <label className="control">
          (ReasonReact.stringToElement("Round to nearest side: "))
          <input
            _type="checkbox"
            checked=(roundToNearestSide ? Js.true_ : Js.false_)
            onChange=(
              self.reduce(
                (event) =>
                  RoundToNearestSide(
                    Js.to_bool(
                      ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##checked
                    )
                  )
              )
            )
          />
        </label>
      </div>
      (
        self.state.controls ?
          ReasonReact.nullElement :
          <a className="github" href="https://github.com/persianturtle/reason-carousel">
            (ReasonReact.stringToElement("Fork me on Github"))
          </a>
      )
    </div>
  }
};
