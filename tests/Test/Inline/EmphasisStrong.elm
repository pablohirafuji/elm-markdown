module Test.Inline.EmphasisStrong exposing (run)


import Html exposing (..)
import Html.Attributes exposing (href, title, src)
import Test.Helpers exposing (..)



-- Based on http://spec.commonmark.org/0.27/#emphasis-and-strong-emphasis


run : List (Output msg)
run =
    [ testEq 328
        []
        "*foo bar*\n"
        [ p []
            [ em []
                [ text "foo bar" ]
            ]
        ]

    , testEq 329
        []
        "a * foo bar*\n"
        [ p []
            [ text "a * foo bar*" ]
        ]

    , testEq 330
        []
        "a*\"foo\"*\n"
        [ p []
            [ text "a*\"foo\"*" ]
        ]

    , testEq 331
        []
        "* a *\n"
        [ p []
            [ text "* a *" ]
        ]

    , testEq 332
        []
        "foo*bar*\n"
        [ p []
            [ text "foo"
            , em []
                [ text "bar" ]
            ]
        ]

    , testEq 333
        []
        "5*6*78\n"
        [ p []
            [ text "5"
            , em []
                [ text "6" ]
            , text "78"
            ]
        ]

    , testEq 334
        []
        "_foo bar_\n"
        [ p []
            [ em []
                [ text "foo bar" ]
            ]
        ]

    , testEq 335
        []
        "_ foo bar_\n"
        [ p []
            [ text "_ foo bar_" ]
        ]

    , testEq 336
        []
        "a_\"foo\"_\n"
        [ p []
            [ text "a_\"foo\"_" ]
        ]

    , testEq 337
        []
        "foo_bar_\n"
        [ p []
            [ text "foo_bar_" ]
        ]

    , testEq 338
        []
        "5_6_78\n"
        [ p []
            [ text "5_6_78" ]
        ]

    , testEq 339
        []
        "пристаням_стремятся_\n"
        [ p []
            [ text "пристаням_стремятся_" ]
        ]

    , testEq 340
        []
        "aa_\"bb\"_cc\n"
        [ p []
            [ text "aa_\"bb\"_cc" ]
        ]

    , testEq 341
        []
        "foo-_(bar)_\n"
        [ p []
            [ text "foo-"
            , em []
                [ text "(bar)" ]
            ]
        ]

    , testEq 342
        []
        "_foo*\n"
        [ p []
            [ text "_foo*" ]
        ]

    , testEq 343
        []
        "*foo bar *\n"
        [ p []
            [ text "*foo bar *" ]
        ]

    , testEq 344
        []
        "*foo bar\n*\n"
        [ p []
            [ text "*foo bar*" ]
        ]

    , testEq 345
        []
        "*(*foo)\n"
        [ p []
            [ text "*(*foo)" ]
        ]

    , testEq 346
        []
        "*(*foo*)*\n"
        [ p []
            [ em []
                [ text "("
                , em []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]

    , testEq 347
        []
        "*foo*bar\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "bar"
            ]
        ]

    , testEq 348
        []
        "_foo bar _\n"
        [ p []
            [ text "_foo bar _" ]
        ]

    , testEq 349
        []
        "_(_foo)\n"
        [ p []
            [ text "_(_foo)" ]
        ]

    , testEq 350
        []
        "_(_foo_)_\n"
        [ p []
            [ em []
                [ text "("
                , em []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]

    , testEq 351
        []
        "_foo_bar\n"
        [ p []
            [ text "_foo_bar" ]
        ]

    , testEq 352
        []
        "_пристаням_стремятся\n"
        [ p []
            [ text "_пристаням_стремятся" ]
        ]

    , testEq 353
        []
        "_foo_bar_baz_\n"
        [ p []
            [ em []
                [ text "foo_bar_baz" ]
            ]
        ]

    , testEq 354
        []
        "_(bar)_.\n"
        [ p []
            [ em []
                [ text "(bar)" ]
            , text "."
            ]
        ]

    , testEq 355
        []
        "**foo bar**\n"
        [ p []
            [ strong []
                [ text "foo bar" ]
            ]
        ]

    , testEq 356
        []
        "** foo bar**\n"
        [ p []
            [ text "** foo bar**" ]
        ]

    , testEq 357
        []
        "a**\"foo\"**\n"
        [ p []
            [ text "a**\"foo\"**" ]
        ]

    , testEq 358
        []
        "foo**bar**\n"
        [ p []
            [ text "foo"
            , strong []
                [ text "bar" ]
            ]
        ]

    , testEq 359
        []
        "__foo bar__\n"
        [ p []
            [ strong []
                [ text "foo bar" ]
            ]
        ]

    , testEq 360
        []
        "__ foo bar__\n"
        [ p []
            [ text "__ foo bar__" ]
        ]

    , testEq 361
        []
        "__\nfoo bar__\n"
        [ p []
            [ text "__foo bar__" ]
        ]

    , testEq 362
        []
        "a__\"foo\"__\n"
        [ p []
            [ text "a__\"foo\"__" ]
        ]

    , testEq 363
        []
        "foo__bar__\n"
        [ p []
            [ text "foo__bar__" ]
        ]

    , testEq 364
        []
        "5__6__78\n"
        [ p []
            [ text "5__6__78" ]
        ]

    , testEq 365
        []
        "пристаням__стремятся__\n"
        [ p []
            [ text "пристаням__стремятся__" ]
        ]

    , testEq 366
        []
        "__foo, __bar__, baz__\n"
        [ p []
            [ strong []
                [ text "foo, "
                , strong []
                    [ text "bar" ]
                , text ", baz"
                ]
            ]
        ]

    , testEq 367
        []
        "foo-__(bar)__\n"
        [ p []
            [ text "foo-"
            , strong []
                [ text "(bar)" ]
            ]
        ]

    , testEq 368
        []
        "**foo bar **\n"
        [ p []
            [ text "**foo bar **" ]
        ]

    , testEq 369
        []
        "**(**foo)\n"
        [ p []
            [ text "**(**foo)" ]
        ]

    , testEq 370
        []
        "*(**foo**)*\n"
        [ p []
            [ em []
                [ text "("
                , strong []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]

    , testEq 371
        []
        "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**\n"
        [ p []
            [ strong []
                [ text "Gomphocarpus ("
                , em []
                    [ text "Gomphocarpus physocarpus" ]
                , text ", syn."
                , em []
                    [ text "Asclepias physocarpa" ]
                , text ")"
                ]
            ]
        ]

    , testEq 372
        []
        "**foo \"*bar*\" foo**\n"
        [ p []
            [ strong []
                [ text "foo \""
                , em []
                    [ text "bar" ]
                , text "\" foo"
                ]
            ]
        ]

    , testEq 373
        []
        "**foo**bar\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "bar"
            ]
        ]

    , testEq 374
        []
        "__foo bar __\n"
        [ p []
            [ text "__foo bar __" ]
        ]

    , testEq 375
        []
        "__(__foo)\n"
        [ p []
            [ text "__(__foo)" ]
        ]

    , testEq 376
        []
        "_(__foo__)_\n"
        [ p []
            [ em []
                [ text "("
                , strong []
                    [ text "foo" ]
                , text ")"
                ]
            ]
        ]

    , testEq 377
        []
        "__foo__bar\n"
        [ p []
            [ text "__foo__bar" ]
        ]

    , testEq 378
        []
        "__пристаням__стремятся\n"
        [ p []
            [ text "__пристаням__стремятся" ]
        ]

    , testEq 379
        []
        "__foo__bar__baz__\n"
        [ p []
            [ strong []
                [ text "foo__bar__baz" ]
            ]
        ]

    , testEq 380
        []
        "__(bar)__.\n"
        [ p []
            [ strong []
                [ text "(bar)" ]
            , text "."
            ]
        ]

    , testEq 381
        []
        "*foo [bar](/url)*\n"
        [ p []
            [ em []
                [ text "foo "
                , a [ href "/url" ]
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 382
        []
        "*foo\nbar*\n"
        [ p []
            [ em []
                [ text "foobar" ]
            ]
        ]

    , testEq 383
        []
        "_foo __bar__ baz_\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]

    , testEq 384
        []
        "_foo _bar_ baz_\n"
        [ p []
            [ em []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]

    , testEq 385
        []
        "__foo_ bar_\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]

    , testEq 386
        []
        "*foo *bar**\n"
        [ p []
            [ em []
                [ text "foo "
                , em []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 387
        []
        "*foo **bar** baz*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]

    , testEq 388
        []
        "*foo**bar**baz*\n"
        [ p []
            [ em []
                [ text "foo"
                , strong []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]

    , testEq 389
        []
        "***foo** bar*\n"
        [ p []
            [ em []
                [ strong []
                    [ text "foo" ]
                , text "bar"
                ]
            ]
        ]

    , testEq 390
        []
        "*foo **bar***\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 391
        []
        "*foo**bar***\n"
        [ p []
            [ em []
                [ text "foo"
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 392
        []
        "*foo **bar *baz* bim** bop*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar "
                    , em []
                        [ text "baz" ]
                    , text "bim"
                    ]
                , text "bop"
                ]
            ]
        ]

    , testEq 393
        []
        "*foo [*bar*](/url)*\n"
        [ p []
            [ em []
                [ text "foo "
                , a [ href "/url" ]
                    [ em []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]

    , testEq 394
        []
        "** is not an empty emphasis\n"
        [ p []
            [ text "** is not an empty emphasis" ]
        ]

    , testEq 395
        []
        "**** is not an empty strong emphasis\n"
        [ p []
            [ text "**** is not an empty strong emphasis" ]
        ]

    , testEq 396
        []
        "**foo [bar](/url)**\n"
        [ p []
            [ strong []
                [ text "foo "
                , a [ href "/url" ]
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 397
        []
        "**foo\nbar**\n"
        [ p []
            [ strong []
                [ text "foo\nbar" ]
            ]
        ]

    , testEq 398
        []
        "__foo _bar_ baz__\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text " baz"
                ]
            ]
        ]

    , testEq 399
        []
        "__foo __bar__ baz__\n"
        [ p []
            [ strong []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                , text " baz"
                ]
            ]
        ]

    , testEq 400
        []
        "____foo__ bar__\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                , text " bar"
                ]
            ]
        ]

    , testEq 401
        []
        "**foo **bar****\n"
        [ p []
            [ strong []
                [ text "foo "
                , strong []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 402
        []
        "**foo *bar* baz**\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                , text " baz"
                ]
            ]
        ]

    , testEq 403
        []
        "**foo*bar*baz**\n"
        [ p []
            [ strong []
                [ text "foo"
                , em []
                    [ text "bar" ]
                , text "baz"
                ]
            ]
        ]

    , testEq 404
        []
        "***foo* bar**\n"
        [ p []
            [ strong []
                [ em []
                    [ text "foo" ]
                , text " bar"
                ]
            ]
        ]

    , testEq 405
        []
        "**foo *bar***\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar" ]
                ]
            ]
        ]

    , testEq 406
        []
        "**foo *bar **baz**\nbim* bop**\n"
        [ p []
            [ strong []
                [ text "foo "
                , em []
                    [ text "bar "
                    , strong []
                        [ text "baz" ]
                    , text "\nbim"
                    ]
                , text " bop"
                ]
            ]
        ]

    , testEq 407
        []
        "**foo [*bar*](/url)**\n"
        [ p []
            [ strong []
                [ text "foo "
                , a [ href "/url" ]
                    [ em []
                        [ text "bar" ]
                    ]
                ]
            ]
        ]

    , testEq 408
        []
        "__ is not an empty emphasis\n"
        [ p []
            [ text "__ is not an empty emphasis" ]
        ]

    , testEq 409
        []
        "____ is not an empty strong emphasis\n"
        [ p []
            [ text "____ is not an empty strong emphasis" ]
        ]

    , testEq 410
        []
        "foo ***\n"
        [ p []
            [ text "foo ***" ]
        ]

    , testEq 411
        []
        "foo *\\**\n"
        [ p []
            [ text "foo "
            , em []
                [ text "*" ]
            ]
        ]

    , testEq 412
        []
        "foo *_*\n"
        [ p []
            [ text "foo "
            , em []
                [ text "_" ]
            ]
        ]

    , testEq 413
        []
        "foo *****\n"
        [ p []
            [ text "foo *****" ]
        ]

    , testEq 414
        []
        "foo **\\***\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "*" ]
            ]
        ]

    , testEq 415
        []
        "foo **_**\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "_" ]
            ]
        ]

    , testEq 416
        []
        "**foo*\n"
        [ p []
            [ text "*"
            , em []
                [ text "foo" ]
            ]
        ]

    , testEq 417
        []
        "*foo**\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "*"
            ]
        ]

    , testEq 418
        []
        "***foo**\n"
        [ p []
            [ text "*"
            , strong []
                [ text "foo" ]
            ]
        ]

    , testEq 419
        []
        "****foo*\n"
        [ p []
            [ text "***"
            , em []
                [ text "foo" ]
            ]
        ]

    , testEq 420
        []
        "**foo***\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "*"
            ]
        ]

    , testEq 421
        []
        "*foo****\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "***"
            ]
        ]

    , testEq 422
        []
        "foo ___\n"
        [ p []
            [ text "foo ___" ]
        ]

    , testEq 423
        []
        "foo _\\__\n"
        [ p []
            [ text "foo "
            , em []
                [ text "_" ]
            ]
        ]

    , testEq 424
        []
        "foo _*_\n"
        [ p []
            [ text "foo "
            , em []
                [ text "*" ]
            ]
        ]

    , testEq 425
        []
        "foo _____\n"
        [ p []
            [ text "foo _____" ]
        ]

    , testEq 426
        []
        "foo __\\___\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "_" ]
            ]
        ]

    , testEq 427
        []
        "foo __*__\n"
        [ p []
            [ text "foo "
            , strong []
                [ text "*" ]
            ]
        ]

    , testEq 428
        []
        "__foo_\n"
        [ p []
            [ text "_"
            , em []
                [ text "foo" ]
            ]
        ]

    , testEq 429
        []
        "_foo__\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "_"
            ]
        ]

    , testEq 430
        []
        "___foo__\n"
        [ p []
            [ text "_"
            , strong []
                [ text "foo" ]
            ]
        ]

    , testEq 431
        []
        "____foo_\n"
        [ p []
            [ text "___"
            , em []
                [ text "foo" ]
            ]
        ]

    , testEq 432
        []
        "__foo___\n"
        [ p []
            [ strong []
                [ text "foo" ]
            , text "_"
            ]
        ]

    , testEq 433
        []
        "_foo____\n"
        [ p []
            [ em []
                [ text "foo" ]
            , text "___"
            ]
        ]

    , testEq 434
        []
        "**foo**\n"
        [ p []
            [ strong []
                [ text "foo" ]
            ]
        ]

    , testEq 435
        []
        "*_foo_*\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]

    , testEq 436
        []
        "__foo__\n"
        [ p []
            [ strong []
                [ text "foo" ]
            ]
        ]

    , testEq 437
        []
        "_*foo*_\n"
        [ p []
            [ em []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]

    , testEq 438
        []
        "****foo****\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                ]
            ]
        ]

    , testEq 439
        []
        "____foo____\n"
        [ p []
            [ strong []
                [ strong []
                    [ text "foo" ]
                ]
            ]
        ]

    , testEq 440
        []
        "******foo******\n"
        [ p []
            [ strong []
                [ strong []
                    [ strong []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]

    , testEq 441
        []
        "***foo***\n"
        [ p []
            [ strong []
                [ em []
                    [ text "foo" ]
                ]
            ]
        ]

    , testEq 442
        []
        "_____foo_____\n"
        [ p []
            [ strong []
                [ strong []
                    [ em []
                        [ text "foo" ]
                    ]
                ]
            ]
        ]

    , testEq 443
        []
        "*foo _bar* baz_\n"
        [ p []
            [ em []
                [ text "foo _bar" ]
            , text " baz_"
            ]
        ]

    , testEq 444
        []
        "*foo __bar *baz bim__ bam*\n"
        [ p []
            [ em []
                [ text "foo "
                , strong []
                    [ text "bar *baz bim" ]
                , text " bam"
                ]
            ]
        ]

    , testEq 445
        []
        "**foo **bar baz**\n"
        [ p []
            [ text "**foo "
            , strong []
                [ text "bar baz" ]
            ]
        ]

    , testEq 446
        []
        "*foo *bar baz*\n"
        [ p []
            [ text "*foo "
            , em []
                [ text "bar baz" ]
            ]
        ]

    , testEq 447
        []
        "*[bar*](/url)\n"
        [ p []
            [ text "*"
            , a [ href "/url" ]
                [ text "bar*" ]
            ]
        ]

    , testEq 448
        []
        "_foo [bar_](/url)\n"
        [ p []
            [ text "_foo "
            , a [ href "/url" ]
                [ text "bar_" ]
            ]
        ]

    , testEq 449
        []
        "*<img src=\"foo\" title=\"*\"/>\n"
        [ p []
            [ text "*"
            , img [ src "foo", title "*" ]
                []
            ]
        ]

    , testEq 450
        []
        "**<a href=\"**\">\n"
        []

    , testEq 451
        []
        "__<a href=\"__\">\n"
        []

    , testEq 452
        []
        "*a `*`*\n"
        [ p []
            [ em []
                [ text "a "
                , code []
                    [ text "*" ]
                ]
            ]
        ]

    , testEq 453
        []
        "_a `_`_\n"
        [ p []
            [ em []
                [ text "a "
                , code []
                    [ text "_" ]
                ]
            ]
        ]

    , testEq 454
        []
        "**a<http://foo.bar/?q=**>\n"
        [ p []
            [ text "**a"
            , a [ href "http://foo.bar/?q=**" ]
                [ text "http://foo.bar/?q=**" ]
            ]
        ]

    , testEq 455
        []
        "__a<http://foo.bar/?q=__>\n"
        [ p []
            [ text "__a"
            , a [ href "http://foo.bar/?q=__" ]
                [ text "http://foo.bar/?q=__" ]
            ]
        ]
    ]