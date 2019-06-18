[%bs.raw {|require('../../../src/Carousel.scss')|}];

let component = ReasonReact.statelessComponent("Carousel");

let create_sides = (sides, radius) =>
  Array.init(
    sides,
    index => {
      let css =
        "rotate3d(0, 1, 0,"
        ++ Js.Float.toString(
             360.0 *. float_of_int(index) /. float_of_int(sides),
           )
        ++ "deg) translate3d(0, 0,"
        ++ Js.Float.toString(radius)
        ++ "0vw)";
      <figure
        key={string_of_int(index)}
        style={ReactDOMRe.Style.make(~transform=css, ())}>
        {ReasonReact.string(string_of_int(index + 1))}
      </figure>;
    },
  );

let make = (~sides, ~radius, ~transform, _children) => {
  ...component,
  render: _self =>
    <section id="carousel" style={ReactDOMRe.Style.make(~transform, ())}>
      {ReasonReact.array(create_sides(sides, radius))}
    </section>,
};