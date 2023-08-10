# Aim

Aim は、**View-Model-Update**パターンの ReasonML 向けの UI ライブラリです。

View を宣言し、それをいつでも好きな時に更新できます。

Aim は仮想 DOM を使用していません。

代わりに動的な部分を明示し、状態の更新と同時に実 DOM に反映します。

# カウンターアプリ

シンプルなカウンターアプリケーションの例です。

```javascript
open Webapi.Dom;

module Counter = {
  let increment = count => count + 1;

  type actions =
    | Increment;

  let make = () => {
    Aim.(
      // View
      component(
        [
          html(
            "div",
            [],
            [
              html("span", [], [text_(count => Js.Int.toString(count))]),
              html(
                "button",
                [event("click", (e, dispatch) => dispatch(Increment))],
                [text("+")],
              ),
            ],
          ),
        ],
        // 初期化。update関数の引数に状態を渡すことで、Viewが更新されます。
        update => update(0),
        (action, update, count) => {
          // click イベントなどで、dispatch関数が実行されると、呼ばれます
          switch (action) {
          | Increment => update(count + 1)
          }
        },
      )
    );
  };
};

let body = Document.querySelector("body", document);

switch (body) {
| Some(body) =>
  let counter = Counter.make();

  Aim.render(counter, body);
| None =>
  exception Body_NotFound;
  raise(Body_NotFound);
};
```

# 使い方

## `render`と`component`

`render`は、`component`関数の結果を用いて HTMLElement に DOM を描画します。

```javascript
render(
  component(
    [html("div", [], [],
    _ => (),
    (_, _, _) => (),
  ),
  // Dom.element
  container
);
```

`component`関数の第１引数は、要素の配列です。

要素の定義の仕方を説明します。

## 要素の定義

### `html(tagName, attributes, children)`

`html`関数は、HTMLElement を定義するための関数です。

### `text(content)`

`text`関数は、TextNode を定義するための関数です。

### `attr(name, value)`

`attr`関数は、属性を定義します。

これらの関数を使った例です。

```javascript
render(
  component(
    [html("button", [attr("type", "button")], [text("button")])],
    _ => (),
    (_, _, _) => (),
  ),
  container
);
```

## 論理属性

### `boolAttr(name, trueOrFalse)`

`checked`や`selected`などの論理属性を定義します。

```javascript
render(
  component(
    [html("input", [attr("type", "checkbox"), boolAttr("checked", true)], [])],
    _ => (),
    (_, _, _) => (),
  ),
  container
);
```

## 状態に対して動的な要素と初期化

状態に対して動的な`TextNode`を定義するための`text_`関数を使った例です。

この例で`text_`は型推論により、`int => string`のシグネチャを持つ関数を引数に取ります。

```javascript
render(
  component(
    [
      html("button", [], [text("button")]),
      html("span", [], [
        // 状態に対する関数
        text_(count => Js.Int.toString(count))
      ]),
    ],
    // `update`関数を用いて、初期値０でViewを更新する
    update => update(0),
    (_, _, _) => (),
  ),
  container
);
```

`component`関数の第２引数に渡される関数は、最初に１回だけ呼ばれます。

そして、部分的な DOM 更新を行う`update`関数を引数に取ります。

## イベント

### `event(name, handler)`

`event`関数は、イベントを定義します。

Aim のイベントハンドラーでは、`dispatch`関数を常に受け取り、*action*を発信できます。

*action*のハンドリングは、`component`関数の第３引数で行われます。

```javascript
type actions =
  | Increment;

render(
  component(
    [
      html(
        "button",
        // イベントハンドラの第２引数に`dispatch`関数を受け取ります
        [event("click", (e, dispatch) => dispatch(Increment))],
        [text("button")],
      ),
      html("span", [], [text_(count => Js.Int.toString(count))]),
    ],
    update => update(0),
    // `dispatch`された`action`が渡されます。
    // `update`関数はViewの更新、`current`は現在の状態値です。
    (action, update, current) => {
      switch (action) {
      | Increment => update(current + 1)
      }
    },
  ),
  container
);
```

`text_`以外の状態に対して動的な要素の定義について説明します。

## 状態に対して動的な要素

### `attr_(name, state => value)`

属性を動的にします。

以下の例では真偽値に対して、表示と非表示を切り替えています。

```javascript
type actions =
  | Switch;

render(
  component(
    [
      html(
        "button",
        [event("click", (e, dispatch) => dispatch(Switch))],
        [text("button")],
      ),
      html(
        "span",
        // style属性が動的に切り替わります
        [attr_("style", show => show ? "" : "display:none")],
        [text("text")],
      ),
    ],
    update => update(true),
    (action, update, current) => {
      switch (action) {
      | Switch => update(!current)
      }
    },
  ),
  container
);
```

### `boolAttr_(name, state => trueOrFalse)`

論理属性を動的にします。

以下の例では、ボタンを押すとチェックボックスのオンオフが切り替わります。

```javascript
type actions =
  | Switch;

render(
  component(
    [
      html(
        "button",
        [event("click", (e, dispatch) => dispatch(Switch))],
        [text("button")],
      ),
      html(
        "input",
        [
          attr("type", "checkbox"),
          // 動的な論理属性
          boolAttr_("checked", checked => checked),
        ],
        [],
      ),
    ],
    update => update(true),
    (action, update, checked) => {
      switch (action) {
      | Switch => update(!checked)
      }
    },
  ),
  container
);
```

### `node_(iterator, keySelector, nodeSelector)`

最後に、`node_`関数を説明します。

この関数は、動的な数の要素をレンダリングする際に利用します。

`iterator`は、状態に対する配列を作成する関数です。

`keySelector`は、`iterator`で作られた配列のそれぞれに対する一意なキーを作る関数です。

`nodeSelector`は、`iterator`で作られた配列のそれぞれに対する要素を作る関数です。

この例では、１秒ごとに要素数が減っていきます。

```javascript
render(
  component(
    <>
      {nodes_(
         // １秒ごとに`count`が増え、配列がフィルタされます
         count => [|1, 2, 3, 4, 5|] |> Js.Array.filter(x => x > count - 1),
         // 数値をキーにします
         (num, i) => Js.Int.toString(num),
         // 動的に`count * num`を計算した結果が反映されます。
         (num, i) =>
           <span> {text_(count => Js.Int.toString(count * num))} </span>,
       )}
    </>,
    update => {
      // タイマーをセットします。
      let rec increment = count => {
      　Js.Global.setTimeout(
          () => {
            update(count);

            if (count <= 5) {
              increment(count + 1);
            };
          },
          1000,
        );

        ();
      };

      increment(1);
    },
    (_, _, _) => {()},
  ),
  container
);
```

## SVG

### `svg(tagName, attributes, children)`

`svg`関数は、SVGElement を定義するための関数で、使い方は`html`関数と同様です。
