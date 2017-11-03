[%bs.raw {|require('./carousel.css')|}];

let component = ReasonReact.statelessComponent("Carousel");

let create_sides = (sides, radius) =>
  Array.init(
    sides,
    (index) => {
      let css =
        "rotate3d(0, 1, 0,"
        ++ (
          string_of_float(360.0 *. float_of_int(index) /. float_of_int(sides))
          ++ ("0deg) translate3d(0, 0," ++ (string_of_float(radius) ++ "0vw)"))
        );
      <figure key=(string_of_int(index)) style=(ReactDOMRe.Style.make(~transform=css, ()))>
        (ReasonReact.stringToElement(string_of_int(index + 1)))
      </figure>
    }
  );

let make = (~sides, ~radius, ~transform, _children) => {
  ...component,
  render: (_self) =>
    <section id="carousel" style=(ReactDOMRe.Style.make(~transform, ()))>
      (ReasonReact.arrayToElement(create_sides(sides, radius)))
    </section>
};
