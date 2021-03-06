// Generated by purs bundle 0.12.3
var PS = {};
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];                 
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];                 
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Bifoldable = PS["Data.Bifoldable"];
  var Data_Bifunctor = PS["Data.Bifunctor"];
  var Data_Bitraversable = PS["Data.Bitraversable"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_FoldableWithIndex = PS["Data.FoldableWithIndex"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_FunctorWithIndex = PS["Data.FunctorWithIndex"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_TraversableWithIndex = PS["Data.TraversableWithIndex"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];                 
  var Left = (function () {
      function Left(value0) {
          this.value0 = value0;
      };
      Left.create = function (value0) {
          return new Left(value0);
      };
      return Left;
  })();
  var Right = (function () {
      function Right(value0) {
          this.value0 = value0;
      };
      Right.create = function (value0) {
          return new Right(value0);
      };
      return Right;
  })();
  var functorEither = new Data_Functor.Functor(function (f) {
      return function (m) {
          if (m instanceof Left) {
              return new Left(m.value0);
          };
          if (m instanceof Right) {
              return new Right(f(m.value0));
          };
          throw new Error("Failed pattern match at Data.Either (line 38, column 8 - line 38, column 52): " + [ m.constructor.name ]);
      };
  });
  var either = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return v(v2.value0);
              };
              if (v2 instanceof Right) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["functorEither"] = functorEither;
})(PS["Data.Either"] = PS["Data.Either"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Effect"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Prelude = PS["Prelude"];                 
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applyEffect"] = applyEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Either = PS["Data.Either"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Unit = PS["Data.Unit"];
  var Effect = PS["Effect"];
  var Effect_Exception = PS["Effect.Exception"];
  var Prelude = PS["Prelude"];                 
  var MonadThrow = function (Monad0, throwError) {
      this.Monad0 = Monad0;
      this.throwError = throwError;
  };
  var throwError = function (dict) {
      return dict.throwError;
  };
  exports["throwError"] = throwError;
  exports["MonadThrow"] = MonadThrow;
})(PS["Control.Monad.Error.Class"] = PS["Control.Monad.Error.Class"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Prelude = PS["Prelude"];                 
  var MonadTrans = function (lift) {
      this.lift = lift;
  };
  var lift = function (dict) {
      return dict.lift;
  };
  exports["lift"] = lift;
  exports["MonadTrans"] = MonadTrans;
})(PS["Control.Monad.Trans.Class"] = PS["Control.Monad.Trans.Class"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Monad = PS["Control.Monad"];
  var Control_Monad_Cont_Class = PS["Control.Monad.Cont.Class"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var Control_Monad_Reader_Class = PS["Control.Monad.Reader.Class"];
  var Control_Monad_Rec_Class = PS["Control.Monad.Rec.Class"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = PS["Control.Monad.Trans.Class"];
  var Control_Monad_Writer_Class = PS["Control.Monad.Writer.Class"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Either = PS["Data.Either"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Newtype = PS["Data.Newtype"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Tuple = PS["Data.Tuple"];
  var Effect_Class = PS["Effect.Class"];
  var Prelude = PS["Prelude"];                 
  var ExceptT = function (x) {
      return x;
  };
  var runExceptT = function (v) {
      return v;
  };          
  var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
      return function (m) {
          return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
              return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v));
          });
      };
  });
  var mapExceptT = function (f) {
      return function (v) {
          return f(v);
      };
  };
  var functorExceptT = function (dictFunctor) {
      return new Data_Functor.Functor(function (f) {
          return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
      });
  };
  var monadExceptT = function (dictMonad) {
      return new Control_Monad.Monad(function () {
          return applicativeExceptT(dictMonad);
      }, function () {
          return bindExceptT(dictMonad);
      });
  };
  var bindExceptT = function (dictMonad) {
      return new Control_Bind.Bind(function () {
          return applyExceptT(dictMonad);
      }, function (v) {
          return function (k) {
              return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function ($97) {
                  return Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($97));
              })(function (a) {
                  var v1 = k(a);
                  return v1;
              }));
          };
      });
  };
  var applyExceptT = function (dictMonad) {
      return new Control_Apply.Apply(function () {
          return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
      }, Control_Monad.ap(monadExceptT(dictMonad)));
  };
  var applicativeExceptT = function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
          return applyExceptT(dictMonad);
      }, function ($98) {
          return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Right.create($98)));
      });
  };
  var monadThrowExceptT = function (dictMonad) {
      return new Control_Monad_Error_Class.MonadThrow(function () {
          return monadExceptT(dictMonad);
      }, function ($102) {
          return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($102)));
      });
  };
  exports["ExceptT"] = ExceptT;
  exports["runExceptT"] = runExceptT;
  exports["mapExceptT"] = mapExceptT;
  exports["functorExceptT"] = functorExceptT;
  exports["applyExceptT"] = applyExceptT;
  exports["applicativeExceptT"] = applicativeExceptT;
  exports["bindExceptT"] = bindExceptT;
  exports["monadExceptT"] = monadExceptT;
  exports["monadTransExceptT"] = monadTransExceptT;
  exports["monadThrowExceptT"] = monadThrowExceptT;
})(PS["Control.Monad.Except.Trans"] = PS["Control.Monad.Except.Trans"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setFillStyle = function(ctx) {
      return function(style) {
          return function() {
              ctx.fillStyle = style;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
      };
  };

  exports.fill = function(ctx) {
      return function() {
          ctx.fill();
      };
  };

  exports.arc = function(ctx) {
      return function(a) {
          return function() {
              ctx.arc(a.x, a.y, a.radius, a.start, a.end);
          };
      };
  };

  exports.rect = function(ctx) {
      return function(r) {
          return function() {
              ctx.rect(r.x, r.y, r.width, r.height);
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Effect = PS["Effect"];
  var Effect_Exception_Unsafe = PS["Effect.Exception.Unsafe"];
  var Prelude = PS["Prelude"];
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  var fillPath = function (ctx) {
      return function (path) {
          return function __do() {
              var v = $foreign.beginPath(ctx)();
              var v1 = path();
              var v2 = $foreign.fill(ctx)();
              return v1;
          };
      };
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["fillPath"] = fillPath;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["setFillStyle"] = $foreign.setFillStyle;
  exports["arc"] = $foreign.arc;
  exports["rect"] = $foreign.rect;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";              

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Math"];
  exports["pi"] = $foreign.pi;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});
(function(exports) {
    "use strict";

  exports.alert = function (str) {
    return function (window) {
      return function () {
        window.alert(str);
        return {};
      };
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Web.HTML.Window"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Newtype = PS["Data.Newtype"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Ord = PS["Data.Ord"];
  var Effect = PS["Effect"];
  var Prelude = PS["Prelude"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var Web_Event_EventTarget = PS["Web.Event.EventTarget"];
  var Web_HTML_HTMLDocument = PS["Web.HTML.HTMLDocument"];
  var Web_HTML_History = PS["Web.HTML.History"];
  var Web_HTML_Location = PS["Web.HTML.Location"];
  var Web_HTML_Navigator = PS["Web.HTML.Navigator"];
  var Web_Storage_Storage = PS["Web.Storage.Storage"];
  exports["alert"] = $foreign.alert;
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var $foreign = PS["Web.HTML"];
  var Effect = PS["Effect"];
  var Web_HTML_HTMLAnchorElement = PS["Web.HTML.HTMLAnchorElement"];
  var Web_HTML_HTMLAreaElement = PS["Web.HTML.HTMLAreaElement"];
  var Web_HTML_HTMLAudioElement = PS["Web.HTML.HTMLAudioElement"];
  var Web_HTML_HTMLBRElement = PS["Web.HTML.HTMLBRElement"];
  var Web_HTML_HTMLBaseElement = PS["Web.HTML.HTMLBaseElement"];
  var Web_HTML_HTMLBodyElement = PS["Web.HTML.HTMLBodyElement"];
  var Web_HTML_HTMLButtonElement = PS["Web.HTML.HTMLButtonElement"];
  var Web_HTML_HTMLCanvasElement = PS["Web.HTML.HTMLCanvasElement"];
  var Web_HTML_HTMLDListElement = PS["Web.HTML.HTMLDListElement"];
  var Web_HTML_HTMLDataElement = PS["Web.HTML.HTMLDataElement"];
  var Web_HTML_HTMLDataListElement = PS["Web.HTML.HTMLDataListElement"];
  var Web_HTML_HTMLDivElement = PS["Web.HTML.HTMLDivElement"];
  var Web_HTML_HTMLDocument = PS["Web.HTML.HTMLDocument"];
  var Web_HTML_HTMLElement = PS["Web.HTML.HTMLElement"];
  var Web_HTML_HTMLEmbedElement = PS["Web.HTML.HTMLEmbedElement"];
  var Web_HTML_HTMLFieldSetElement = PS["Web.HTML.HTMLFieldSetElement"];
  var Web_HTML_HTMLFormElement = PS["Web.HTML.HTMLFormElement"];
  var Web_HTML_HTMLHRElement = PS["Web.HTML.HTMLHRElement"];
  var Web_HTML_HTMLHeadElement = PS["Web.HTML.HTMLHeadElement"];
  var Web_HTML_HTMLHeadingElement = PS["Web.HTML.HTMLHeadingElement"];
  var Web_HTML_HTMLIFrameElement = PS["Web.HTML.HTMLIFrameElement"];
  var Web_HTML_HTMLImageElement = PS["Web.HTML.HTMLImageElement"];
  var Web_HTML_HTMLInputElement = PS["Web.HTML.HTMLInputElement"];
  var Web_HTML_HTMLKeygenElement = PS["Web.HTML.HTMLKeygenElement"];
  var Web_HTML_HTMLLIElement = PS["Web.HTML.HTMLLIElement"];
  var Web_HTML_HTMLLabelElement = PS["Web.HTML.HTMLLabelElement"];
  var Web_HTML_HTMLLegendElement = PS["Web.HTML.HTMLLegendElement"];
  var Web_HTML_HTMLLinkElement = PS["Web.HTML.HTMLLinkElement"];
  var Web_HTML_HTMLMapElement = PS["Web.HTML.HTMLMapElement"];
  var Web_HTML_HTMLMediaElement = PS["Web.HTML.HTMLMediaElement"];
  var Web_HTML_HTMLMetaElement = PS["Web.HTML.HTMLMetaElement"];
  var Web_HTML_HTMLMeterElement = PS["Web.HTML.HTMLMeterElement"];
  var Web_HTML_HTMLModElement = PS["Web.HTML.HTMLModElement"];
  var Web_HTML_HTMLOListElement = PS["Web.HTML.HTMLOListElement"];
  var Web_HTML_HTMLObjectElement = PS["Web.HTML.HTMLObjectElement"];
  var Web_HTML_HTMLOptGroupElement = PS["Web.HTML.HTMLOptGroupElement"];
  var Web_HTML_HTMLOptionElement = PS["Web.HTML.HTMLOptionElement"];
  var Web_HTML_HTMLOutputElement = PS["Web.HTML.HTMLOutputElement"];
  var Web_HTML_HTMLParagraphElement = PS["Web.HTML.HTMLParagraphElement"];
  var Web_HTML_HTMLParamElement = PS["Web.HTML.HTMLParamElement"];
  var Web_HTML_HTMLPreElement = PS["Web.HTML.HTMLPreElement"];
  var Web_HTML_HTMLProgressElement = PS["Web.HTML.HTMLProgressElement"];
  var Web_HTML_HTMLQuoteElement = PS["Web.HTML.HTMLQuoteElement"];
  var Web_HTML_HTMLScriptElement = PS["Web.HTML.HTMLScriptElement"];
  var Web_HTML_HTMLSelectElement = PS["Web.HTML.HTMLSelectElement"];
  var Web_HTML_HTMLSourceElement = PS["Web.HTML.HTMLSourceElement"];
  var Web_HTML_HTMLSpanElement = PS["Web.HTML.HTMLSpanElement"];
  var Web_HTML_HTMLStyleElement = PS["Web.HTML.HTMLStyleElement"];
  var Web_HTML_HTMLTableCaptionElement = PS["Web.HTML.HTMLTableCaptionElement"];
  var Web_HTML_HTMLTableCellElement = PS["Web.HTML.HTMLTableCellElement"];
  var Web_HTML_HTMLTableColElement = PS["Web.HTML.HTMLTableColElement"];
  var Web_HTML_HTMLTableDataCellElement = PS["Web.HTML.HTMLTableDataCellElement"];
  var Web_HTML_HTMLTableElement = PS["Web.HTML.HTMLTableElement"];
  var Web_HTML_HTMLTableHeaderCellElement = PS["Web.HTML.HTMLTableHeaderCellElement"];
  var Web_HTML_HTMLTableRowElement = PS["Web.HTML.HTMLTableRowElement"];
  var Web_HTML_HTMLTableSectionElement = PS["Web.HTML.HTMLTableSectionElement"];
  var Web_HTML_HTMLTemplateElement = PS["Web.HTML.HTMLTemplateElement"];
  var Web_HTML_HTMLTextAreaElement = PS["Web.HTML.HTMLTextAreaElement"];
  var Web_HTML_HTMLTimeElement = PS["Web.HTML.HTMLTimeElement"];
  var Web_HTML_HTMLTitleElement = PS["Web.HTML.HTMLTitleElement"];
  var Web_HTML_HTMLTrackElement = PS["Web.HTML.HTMLTrackElement"];
  var Web_HTML_HTMLUListElement = PS["Web.HTML.HTMLUListElement"];
  var Web_HTML_HTMLVideoElement = PS["Web.HTML.HTMLVideoElement"];
  var Web_HTML_History = PS["Web.HTML.History"];
  var Web_HTML_Location = PS["Web.HTML.Location"];
  var Web_HTML_Navigator = PS["Web.HTML.Navigator"];
  var Web_HTML_Window = PS["Web.HTML.Window"];
  exports["window"] = $foreign.window;
})(PS["Web.HTML"] = PS["Web.HTML"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Monad_Except = PS["Control.Monad.Except"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Effect = PS["Effect"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var $$Math = PS["Math"];
  var Prelude = PS["Prelude"];
  var Web_HTML = PS["Web.HTML"];
  var Web_HTML_Window = PS["Web.HTML.Window"];                 
  var reportResult = function (window) {
      return function (v) {
          if (v instanceof Data_Either.Right) {
              return Web_HTML_Window.alert(v.value0)(window);
          };
          if (v instanceof Data_Either.Left) {
              return Web_HTML_Window.alert(v.value0)(window);
          };
          throw new Error("Failed pattern match at RayTracer.Canvas (line 22, column 1 - line 22, column 62): " + [ window.constructor.name, v.constructor.name ]);
      };
  };
  exports["reportResult"] = reportResult;
})(PS["RayTracer.Canvas"] = PS["RayTracer.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.12.3
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var Control_Monad_Except = PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = PS["Control.Monad.Except.Trans"];
  var Control_Monad_Trans_Class = PS["Control.Monad.Trans.Class"];
  var Data_Either = PS["Data.Either"];
  var Data_EuclideanRing = PS["Data.EuclideanRing"];
  var Data_Function = PS["Data.Function"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semiring = PS["Data.Semiring"];
  var Effect = PS["Effect"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var $$Math = PS["Math"];
  var Prelude = PS["Prelude"];
  var RayTracer_Canvas = PS["RayTracer.Canvas"];
  var Web_HTML = PS["Web.HTML"];
  var Web_HTML_Window = PS["Web.HTML.Window"];                 
  var writeToCanvas = Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.getCanvasElementById("canvas")))(function (v) {
      if (v instanceof Data_Maybe.Nothing) {
          return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Effect.monadEffect))("Unable to find canvas");
      };
      if (v instanceof Data_Maybe.Just) {
          return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.getContext2D(v.value0)))(function (v1) {
              return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.setFillStyle(v1)("#0000FF")))(function (v2) {
                  return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.fillPath(v1)(Graphics_Canvas.rect(v1)({
                      x: 250.0,
                      y: 250.0,
                      width: 100.0,
                      height: 100.0
                  }))))(function (v3) {
                      return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.setFillStyle(v1)("#00FF00")))(function (v4) {
                          return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Effect.monadEffect)(Graphics_Canvas.fillPath(v1)(Graphics_Canvas.arc(v1)({
                              x: 225.0,
                              y: 225.0,
                              radius: 50.0,
                              start: ($$Math.pi * 5.0) / 8.0,
                              end: $$Math.pi * 2.0
                          }))))(function (v5) {
                              return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Effect.monadEffect))("That worked!");
                          });
                      });
                  });
              });
          });
      };
      throw new Error("Failed pattern match at Main (line 33, column 3 - line 41, column 12): " + [ v.constructor.name ]);
  });
  var main = function __do() {
      var v = Web_HTML.window();
      return Control_Bind.bind(Effect.bindEffect)(Control_Monad_Except_Trans.runExceptT(writeToCanvas))(RayTracer_Canvas.reportResult(v))();
  };
  exports["main"] = main;
  exports["writeToCanvas"] = writeToCanvas;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();