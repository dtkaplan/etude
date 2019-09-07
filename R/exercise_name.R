# Internal functions to Generate a random name for an exercise

animal_words <- c(
  "ant", "aspen", "ash",
  "bear", "bee", "bird", "boy", "beech",
  "buck", "birch",
  "camel", "cat", "cheetah", "chicken", "calf",
  "cow", "crocodile", "child", "crow",
  "deer", "dog", "dolphin", "duck", "doe",
  "eagle", "elm", "elephant",
  "fish", "fly", "fox", "frog", "finch", "falcon", "fir",
  "giraffe", "goat", "goldfish", "girl",
  "hamster",  "horse",
  "kangaroo", "kitten", "kid", "lamb", "lion", "lobster",
  "maple", "monkey", "octopus", "owl", "oak",
  "panda", "pig", "puppy",  "pine", "pollen",
  "rabbit", "rat",
  "seal", "shark", "sheep", "snail", "snake", "spider",
  "squirrel", "spruce", "seaweed",
  "tiger", "turtle",
  "walnut", "wolf", "zebra"
  )

verb_words <- c(
  "beat", "become", "begin", "bend","bet", "bid", "bite",
  "blow",  "break", "bring", "build", "burn", "buy",
  "catch", "chew", "choose",
  "come",  "cost", "cut",
  "dig","dive", "do", "draw",  "dream",  "drive",  "drink",
  "eat",
  "fall", "feel", "fight",
  "find", "fly", "forget", "forgive", "freeze",
  "get", "give", "go", "grow",
  "hang",  "have",  "hear", "hide", "hit",  "hold",  "hurt",
  "iron", "jump",
  "keep",  "know",
  "light", "lay",  "lead", "leave",  "lend",
  "let", "lie", "lose", "look",
  "make", "mean",  "meet",
  "pay","put", "pitch",
  "read","ride", "ring","rise",  "run",
  "say", "sail", "see",  "sell","send",  "show",   "shut",  "sing",
  "sit", "sleep",  "speak", "spend", "stand",   "swim",
  "take", "talk", "teach",
  "tear",  "tell", "think",  "throw", "toss", "trim", "tug", "type",
  "understand", "walk",  "wake",  "wear", "win", "write"
)

everyday_nouns <- c(
  "bed", "blanket", "boat", "book", "bottle", "bowl", "bulb",
  "candy", "canoe", "car", "chair", "clock", "closet", "coat", "cotton",
  "dish", "door", "drawer", "dress",
  "fork", "futon", "fridge", "glasses", "gloves",
  "jacket", "kayak", "kitchen", "knife", "knob",
  "lamp", "linen",
  "magnet", "map", "mug",
  "oven", "painting",
  "pants", "pen", "pencil", "piano", "plant", "plate", "pot", "pan",
  "radio", "ring", "roof", "room", "rug",
  "saucer", "saw", "scarf", "sheet", "ship", "shirt",
  "shoe", "socks", "sofa", "spoon", "stove",
  "table", "tv", "vase", "window")

make_random_id <- function(){
  paste(
    sample(animal_words, 1),
    sample(verb_words, 1),
    sample(everyday_nouns, 1),
    sep = "-"
  )
}
