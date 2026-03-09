| inputString chunkSize result |

'Enter a string: ' display.
inputString := stdin nextLine.
'Enter a chunk size: ' display.
chunkSize := (stdin nextLine) asInteger.

result := OrderedCollection new.

1 to: inputString size by: chunkSize do: [:i |
    | chunk |
    chunk := inputString copyFrom: i to: ((i + chunkSize - 1) min: inputString size).
    result add: chunk.
].

result do: [:chunk | chunk displayNl.].