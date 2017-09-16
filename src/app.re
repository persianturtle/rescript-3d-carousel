[%bs.raw {|require('./app.css')|}];

type state = {
  sides: int,
  friction: float,
  roundToNearestSide: bool
};

type action =
  | Sides int
  | Friction int
  | RoundToNearestSide bool;

let component = ReasonReact.reducerComponent "App";

let make _children => {
  ...component,
  initialState: fun () => {sides: 8, friction: 0.0175, roundToNearestSide: true},
  reducer: fun action state =>
    switch action {
    | Sides side => ReasonReact.Update {...state, sides: side}
    | Friction slider =>
      let friction = 0.045 *. float_of_int slider /. 100.0 +. 0.005;
      ReasonReact.Update {...state, friction}
    | RoundToNearestSide roundToNearestSide => ReasonReact.Update {...state, roundToNearestSide}
    },
  render: fun self => {
    let sides = self.state.sides;
    let friction = self.state.friction;
    let roundToNearestSide = self.state.roundToNearestSide;
    let slider = int_of_float ((friction -. 0.005) *. 100.0 /. 0.045);
    Js.log roundToNearestSide;
    <div>
      <Container sides friction roundToNearestSide />
      <div className="controls">
        <label>
          (ReasonReact.stringToElement "Sides: ")
          <select
            value=(string_of_int sides)
            onChange=(
              self.reduce (
                fun event =>
                  Sides (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value
              )
            )>
            <option> (ReasonReact.stringToElement "3") </option>
            <option> (ReasonReact.stringToElement "4") </option>
            <option> (ReasonReact.stringToElement "5") </option>
            <option> (ReasonReact.stringToElement "6") </option>
            <option> (ReasonReact.stringToElement "7") </option>
            <option> (ReasonReact.stringToElement "8") </option>
            <option> (ReasonReact.stringToElement "9") </option>
            <option> (ReasonReact.stringToElement "10") </option>
            <option> (ReasonReact.stringToElement "11") </option>
            <option> (ReasonReact.stringToElement "12") </option>
            <option> (ReasonReact.stringToElement "13") </option>
            <option> (ReasonReact.stringToElement "14") </option>
            <option> (ReasonReact.stringToElement "15") </option>
          </select>
        </label>
        <label>
          (ReasonReact.stringToElement "Friction: ")
          <input
            _type="range"
            value=(string_of_int slider)
            onChange=(
              self.reduce (
                fun event =>
                  Friction (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value
              )
            )
          />
        </label>
        <label>
          (ReasonReact.stringToElement "Round to nearest side: ")
          <input
            _type="checkbox"
            checked=(roundToNearestSide ? Js.true_ : Js.false_)
            onChange=(
              self.reduce (
                fun event =>
                  RoundToNearestSide (
                    Js.to_bool (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked
                  )
              )
            )
          />
        </label>
      </div>
    </div>
  }
};
