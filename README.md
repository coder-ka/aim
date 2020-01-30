# Aim

Aim は、**View-Model-Update**パターンの ReasonML 向けの UI ライブラリです。

View を宣言し、それをいつでも好きな時に更新できます。

Aim は仮想 DOM を使用していません。

代わりに動的な部分を明示し、状態の更新と同時に実 DOM に反映します。

# シンプルなカウンターコンポーネント

`module`をつかって簡単にコンポーネントを構築できます。

```reasonml
open Webapi.Dom;

module Counter = {
  let increment = count => count + 1;

  let start = (defaultState, container) => {
    open Aim;

    let div = html("div");
    let button = html("button");
    let span = html("span");

    define(
      div([
        span([
          text_(count => Js.Int.toString(count)),
        ]),
        button([
          event_("click", (_, count) =>
            update(increment(count), container)
          ),
          text("+")
        ]),
      ]),
      container,
    );

    update(defaultState, container);
  };
};

let container = Document.querySelector("body", document);

// start counter app in <body>
Counter.start(0, container);
```

簡単にロジックと UI をカプセル化し、`Counter.increment` というロジックのみを単体テストすることもできます。

# API

Aim の API はすべて関数です。

UI も関数で宣言します。

こちらの２つの関数で、View の定義と更新を行います。

- `define`
- `update`

```reasonml
let container = Document.querySelector("body", document);

define(
  html("div", [
    text("now: ")
    // この部分のみが動的に更新されます
    text_(date => Js.Date.toLocaleTimeString(date))
  ]),
  container
)

// 正確ではないですが、１秒ごとに日時が更新されます
Js_global.setInvterval(() => {
  update(Js.Date.make(), container);
}, 1000);
```

`update`はジェネリックな第一引数を受け取れます。

ReasonML の型推論によるチェックも働きます。

要素の定義は、下記の２つを使います。

- `html`
- `svg`

```reasonml
// カリー化されます
let div = html("div");

define(div([
  // 「子」が入ります
]));
```

「子」には、要素に対する付加情報のリストが入ります。

「属性」や「イベント」、「子要素」などです。

ルールとして最後に`_`が付くメソッドは、状態に対して動的です。

- `text`
- `text_`

テキストノードを定義できます。

```reasonml
let div = html("div");

// <div>hello world</div>
div([
  text("hello world"),
  // 状態更新と同時に、更新されます
  text_(state => "")
]);
```

- `attr`
- `attr_`

属性を定義できます。

```reasonml
let div = html("div");

// <div class="hoge" id=""></div>
div([
  attr("class", "hoge"),
  // 状態更新と同時に、更新されます
  attr_("id", state => "")
]);
```

- `boolAttr`
- `boolAttr_`

`checked`や`selected`などの属性を真偽値で定義できます。

```reasonml
let input = html("input");

// <input type="checkbox" checked />
div([attr("type", "checkbox"), boolAttr("checked", true)]);

// <input type="checkbox" checked />
div([
  attr("type", "checkbox"),
  // 状態更新と同時に、更新されます
  boolAttr_("checked", state => state) // true or false
]);
```

- `event`
- `event_`

イベントを定義できます。

```reasonml
let button = html("button");

button([event("click", e => Js.log(e))]);

button([event_("click", (e, state) => update(state + 1, container))]);
```

- `eventWithOptions`
- `eventWithOptions_`

オプション付きで、イベントを定義できます。

```reasonml
let button = html("button");

button([
  eventWithOptions("click", e => Js.log(e), {
    capture: true,
    once: true,
    passive: true
  })
]);

button([
  // 動的な状態更新ができます
  eventWithOptions_("click", (e, state) => update(state + 1, container), {
    capture: true,
    once: true,
    passive: true
  })
]);
```

- `slot`
- `slot_`

要素を受け取り、さらにコンポーネントを入れ子にできます。

```reasonml
module Another {
  let start = el => {
    define(...)
    update(...)
  }
}

let div = html("div");

div([
  slot(el => Another.start(el))
])

div([
  // 状態に対して動的です。
  // これによりAnotherコンポーネントの`define`が２回呼ばれますが、キャッシュによりパスされる結果、`update`による初期化のみが実行されます。
  slot_((el, state) => Another.start(el))
])
```

- `nodes_`

通常の Aim によるテンプレートは静的なので、別のコンポーネントに切り出さなかったり、`attr_`を使い状態に対して`display: none`としない限り、要素を動的にはできず、複数のリストを動的な要素として宣言できません。

そうした場合は、`nodes_`を使ってください。

キーを利用して効率的に差分適用を行います。

```reasonml
let div = html("div");

div([
  nodes_(
    // まず状態に対するリストを定義します
    state => [1, 2, 3, 4, 5] |> Js.List.filter(x => x < state),
    // 一意な文字列キーを生成します
    (num, index) => num,
    // 最後に、要素のファクトリを指定します
    (num, index) => text_(state => num)
  )
]);
```

最初の１回以降、要素数が変わらないなら、`map`を使えます。

```reasonml
let div = html("div");

let children =
  [1, 2, 3, 4, 5]
  |> Js.List.map(x => text_(state => Js.Int.toString(x * state)));

div(children);
```

Aim については、以上です！
