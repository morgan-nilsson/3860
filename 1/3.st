Object subclass: CeeloGame [
    | playerCredits r |

    initialize [
        playerCredits := 100.
        r := Random new.
        ^self
    ]

    rollDice: dice [
        dice at: 1 put: (r between: 1 and: 6).
        dice at: 2 put: (r between: 1 and: 6).
        dice at: 3 put: (r between: 1 and: 6).
        'Dice roll: ' display.
        dice displayNl.
    ]

    evaluateRoll: dice [
        | ret s spare |
        ret := 'Indeterminate'.

        "Trips"
        s := Set withAll: dice.
        ((s size) = 1) ifTrue: [
            ret := 'win'.
        ].

        "4, 5, 6"
        ((s includes: 4) & (s includes: 5) & (s includes: 6)) ifTrue: [
            ret := 'win'.
        ].

        "Pair + value"
        (s size = 2) ifTrue: [
            spare := 0.

            s := Set new.
            s add: (dice at: 1).
            s add: (dice at: 2).

            (s size = 1) ifTrue: [
                spare := dice at: 3
            ] ifFalse: [
                ((dice at: 1) = (dice at: 3)) ifTrue: [
                    spare := dice at: 2.
                ] ifFalse: [
                    spare := dice at: 1.
                ].
            ].

            ret := spare asString.
            (spare = 6) ifTrue: [
                ret := 'win'.
            ].
            (spare = 1) ifTrue: [
                ret := 'lose'.
            ].
        ].

        "1, 2, 3"
        ((s includes: 1) & (s includes: 2) & (s includes: 3)) ifTrue: [
            ret := 'lose'.
        ].

        ^ret
    ]

    rollNonIndeterminate: dice [
        | result |
        self rollDice: dice.
        result := self evaluateRoll: dice.
        result displayNl.
        (result = 'Indeterminate') ifTrue: [
            ^self rollNonIndeterminate: dice
        ].
        ^result
    ]

    playRoll [
        | dice banker player |
        dice := Array new: 3.

        "Banker's turn"
        'Banker rolls:' displayNl.
        banker := self rollNonIndeterminate: dice.

        (banker = 'win') ifTrue: [ ^'lose' ].
        (banker = 'lose') ifTrue: [ ^'win' ].

        "Player's turn"
        'Player rolls:' displayNl.
        player := self rollNonIndeterminate: dice.

        (player = 'win') ifTrue: [ ^'win' ].
        (player = 'lose') ifTrue: [ ^'lose' ].

        "Point based comparison"
        'Banker point: ' display. banker displayNl.
        'Player point: ' display. player displayNl.
        ((banker asNumber) = (player asNumber)) ifTrue: [ ^'tie' ].
        ((banker asNumber) < (player asNumber)) ifTrue: [ ^'win' ].
        ^'lose'
    ]

    getPlayerBetAmount: attempts [
        | betAmount |
        'You have ' display.
        playerCredits display.
        ' credits. Enter bet amount (0 to quit): ' display.
        betAmount := stdin nextLine asNumber.

        ((betAmount < 0) or: [ betAmount > playerCredits ])
            ifTrue: [
                'Invalid bet amount. Try again.' displayNl.
                ^self getPlayerBetAmount: (attempts + 1)
            ]
            ifFalse: [ ^betAmount ]
    ]

    gameLoop [
        | playerBetAmount rollResult |
        (playerCredits <= 0) ifTrue: [
            'You are out of credits. Game over!' displayNl.
            ^self
        ].

        playerBetAmount := self getPlayerBetAmount: 1.

        (playerBetAmount = 0) ifTrue: [
            'Thanks for playing! Final credits: ' display.
            playerCredits displayNl.
            ^self
        ].

        rollResult := self playRoll.
        'Result: ' display.
        rollResult displayNl.

        (rollResult = 'win') ifTrue: [
            'You win!' displayNl.
            playerCredits := playerCredits + playerBetAmount.
        ].
        (rollResult = 'lose') ifTrue: [
            'You lose!' displayNl.
            playerCredits := playerCredits - playerBetAmount.
        ].
        (rollResult = 'tie') ifTrue: [
            'It''s a tie!' displayNl.
        ].

        '' displayNl.
        self gameLoop.
    ]
]

"Start the game"
'You start with 100 credits.' displayNl.
'' displayNl.
game := CeeloGame new initialize.
game gameLoop.
