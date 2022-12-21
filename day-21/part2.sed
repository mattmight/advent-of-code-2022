s/(root): ([a-z]{4}) (\-|\*|\+|\/) ([a-z]{4})/define root(x) { return \2(x) - \4(x) }/g
s/humn: [0-9]+/define humn(x) { return x }/g
s/([a-z]{4}): ([0-9]+)/define \1(x) { return \2 }/g
s/([a-z]{4}): ([a-z]{4}) (\-|\*|\+|\/) ([a-z]{4})/define \1(x) { return \2(x) \3 \4(x) }/g
