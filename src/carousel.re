%raw
{| import "../../../src/Carousel.scss" |};

let create_sides = (team, radius) =>
  Js.Array.mapi(
    (member, index) => {
      let css =
        "rotate3d(0, 1, 0,"
        ++ Js.Float.toString(
             360.0
             *. float_of_int(index)
             /. float_of_int(Js.Array.length(team)),
           )
        ++ "deg) translate3d(0, 0,"
        ++ Js.Float.toString(radius)
        ++ "vw)";
      <figure
        key={string_of_int(index)}
        style={ReactDOMRe.Style.make(~transform=css, ())}>
        {ReasonReact.string(member)}
      </figure>;
    },
    team,
  );

[@react.component]
let make = (~team, ~radius, ~transform) =>
  <section id="carousel" style={ReactDOMRe.Style.make(~transform, ())}>
    {ReasonReact.array(create_sides(team, radius))}
  </section>;
