open Jest;
open Webapi.Dom;

module Elements = {
  open Aim;

  let div = (~children, ()) => html("div", [], children);
  let button = (~id, ~onClick, ~children, ()) =>
    html("button", [attr("id", id), event("click", onClick)], children);

  let span = (~id="", ~children, ()) =>
    html("span", [attr("id", id)], children);
};

let generateArray = () => {
  let arr = Array.make(100, 0) |> Array.map(_ => Js.Math.random_int(0, 999));
  arr;
};
let sampled = generateArray();

module DynamicNodes = {
  open Aim;
  open Elements;

  let filterAndSort = (arr, count) => {
    // filter
    let filtered = arr |> Js.Array.filter(x => x > count);

    // sort
    filtered
    |> Array.sort((cur, next) => count mod 2 === 0 ? cur - next : next - cur);

    // distinct
    filtered
    |> Js.Array.filteri((x, i) => {
         Js.Array.findIndex(y => y === x, filtered) === i
       });
  };

  let createElement = (~children, ()) => {
    component(
      <>
        {nodes_(
           count => {filterAndSort(sampled, count)},
           (num, _) => Js.Int.toString(num),
           (num, _) =>
             <span id={Js.Int.toString(num)}>
               {text_(count => Js.Int.toString((count + 1) * num))}
             </span>,
         )}
      </>,
      update => {
        update(0);
        update(1);
      },
      (_, _, _) => {()},
    );
  };
};

describe("Expect", () => {
  Expect.(
    test("click increment button", () => {
      let body = Document.querySelector("body", document);
      switch (body) {
      | Some(body) =>
        let container = Document.createElement("div", document);
        body |> Element.appendChild(container);
        container |> Element.setAttribute("id", "app");

        // render component
        let container = document |> Document.getElementById("app");
        let container =
          switch (container) {
          | Some(root) => root
          | None =>
            exception RootElement_NotFound;
            raise(RootElement_NotFound);
          };

        Aim.render(<DynamicNodes />, container);

        let spans = container |> Element.querySelectorAll("span");
        let nodes = spans |> Webapi.Dom.NodeList.toArray;
        let texts = nodes |> Array.map(x => Webapi.Dom.Node.textContent(x));

        let count = 1;
        let expected =
          DynamicNodes.filterAndSort(sampled, 1)
          |> Array.map(num => Js.Int.toString((count + 1) * num));

        expect(expected) |> toEqual(texts);
      | None =>
        exception Body_NotFound;
        raise(Body_NotFound);
      };
    })
  )
});
