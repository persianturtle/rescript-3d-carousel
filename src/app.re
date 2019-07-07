[%bs.raw {|require('../../../src/App.scss')|}];

type state = {
  numberOfSides: int,
  amountOfFriction: int,
  shouldRoundToNearestSide: bool,
  shouldShowMobileControls: bool,
};

type action =
  | SetNumberOfSides(int)
  | SetFriction(int)
  | ToggleRoundToNearestSide(bool)
  | ToggleControls(bool);

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | SetNumberOfSides(n) => {
            ...state,
            numberOfSides: state.numberOfSides + n,
          }
        | SetFriction(friction) => {...state, amountOfFriction: friction}
        | ToggleRoundToNearestSide(shouldRoundToNearestSide) => {
            ...state,
            shouldRoundToNearestSide,
          }
        | ToggleControls(shouldShowMobileControls) => {
            ...state,
            shouldShowMobileControls,
          }
        },
      {
        numberOfSides: 8,
        amountOfFriction: 50,
        shouldRoundToNearestSide: true,
        shouldShowMobileControls: false,
      },
    );
  let {
    numberOfSides,
    amountOfFriction,
    shouldRoundToNearestSide,
    shouldShowMobileControls,
  } = state;
  let radius =
    25.0
    /. Js.Math.tan(
         180.0 /. float_of_int(numberOfSides) *. (Js.Math._PI /. 180.0),
       );
  let perspective =
    Js.Float.toString(500.0 /. float_of_int(numberOfSides)) ++ "vw";
  <div>
    <Container
      numberOfSides
      radius
      perspective
      amountOfFriction
      shouldRoundToNearestSide
    />
    {shouldShowMobileControls
       ? ReasonReact.null
       : <a
           className="trigger"
           onClick={_event => dispatch(ToggleControls(true))}>
           {ReasonReact.string("Controls")}
         </a>}
    <div className={shouldShowMobileControls ? "controls active" : "controls"}>
      <button
        className="close"
        onClick={_event => dispatch(ToggleControls(false))}>
        {ReasonReact.string({js|\u00d7|js})}
      </button>
      <div className="control">
        <button
          onClick={_event => dispatch(SetNumberOfSides(-1))}
          disabled={numberOfSides == 3 ? true : false}>
          {ReasonReact.string(" - ")}
        </button>
        {ReasonReact.string("Sides: ")}
        <button onClick={_event => dispatch(SetNumberOfSides(1))}>
          {ReasonReact.string(" + ")}
        </button>
      </div>
      <div className="control">
        {ReasonReact.string("Friction: ")}
        <input
          type_="range"
          value={string_of_int(amountOfFriction)}
          onChange={event =>
            dispatch(SetFriction(ReactEvent.Form.target(event)##value))
          }
        />
      </div>
      <label className="control">
        {ReasonReact.string("Round to nearest side: ")}
        <input
          type_="checkbox"
          checked={shouldRoundToNearestSide ? true : false}
          onChange={event =>
            dispatch(
              ToggleRoundToNearestSide(
                ReactEvent.Form.target(event)##checked,
              ),
            )
          }
        />
      </label>
    </div>
    {shouldShowMobileControls
       ? ReasonReact.null
       : <a
           className="github"
           href="https://github.com/persianturtle/reason-carousel">
           {ReasonReact.string("Fork me on Github")}
         </a>}
  </div>;
};