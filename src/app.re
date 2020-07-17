%raw
{| import "../../../src/App.scss" |};

let team =
  Js.String.split(
    ",",
    [%raw "new URLSearchParams(window.location.search).get('team')"],
  );

[@react.component]
let make = () => {
  <div>
    <Container team amountOfFriction=15 shouldRoundToNearestSide=true />
  </div>;
};
