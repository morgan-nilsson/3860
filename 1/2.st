r := Random new.
sum := 0.
func := [
    rnd := r between: 1 and: 6.
    rnd displayNl.
    sum := sum + rnd.
    (rnd = 1) ifTrue: [
        'You rolled a total of ' display.
        sum display.
        ' before rolling a 1' displayNl.
    ] ifFalse: [
        'Press <Enter> to roll or q to quit' displayNl.
        in := stdin nextLine.
        (in = 'q') ifTrue: [
            'You rolled a total of ' display.
            sum displayNl.
        ] ifFalse: [
            func value.
        ]
    ]
].

func value.