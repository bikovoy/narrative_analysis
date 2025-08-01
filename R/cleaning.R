# Loading Required Libraries

library(tidytext)
library(dplyr)
library(stringr)
library(stopwords)

# Function: Loading and labeling data

load_and_label <- function(file_path, country, conflict) {
  readr::read_csv(file_path) %>%
    mutate(country = country, conflict = conflict)
}

# Loading all speech data with metadata

us_ukraine <- readr::read_csv("data/us_ukraine.csv")
russia_ukraine <- readr::read_csv("data/russia_ukraine.csv")
us_chechnya <- readr::read_csv("data/us_chechnya.csv")
russia_chechnya <- readr::read_csv("data/russia_chechnya.csv")

# Combining all datasets

df <- bind_rows(us_ukraine, russia_ukraine, us_chechnya, russia_chechnya)

# Creating and exporting corpus index for reference

corpus_index <- df %>% select(speaker, date, title, url, country, conflict)
write.csv(corpus_index, "corpus_index.csv", row.names = FALSE)

# Checking document counts by group

df_counts <- df %>% count(country, conflict)
print(df_counts) # Note: Data is imbalanced, normalization handled later

# Defining custom stopwords

custom_stopwords <- c(
  stopwords::stopwords("en"),
  stopwords::stopwords("ru"),
  "said", "will", "also", "president", "statement", "government", "people", "chechnya", "ukraine", "russia", "can", "question", "one", "mr", "going", "well", "say", "see", "get", "make", "want", "first", "lockhart", "just", "like", "go", "today", "much", "right", "way", "united", "states", "us", "country", "countries", "years", "states", "world", "new", "time", "last", "many", "two", "think", "putin", "trump", "russian", "iraq", "secretary", "now", "know", "это", "minister", "donald", "come", "powell", "work", "terry", "something", "thank", "things", "moran", "take", "let", "look", "q", "back", "made", "need", "lot", "still", "even", "year","long", "good", "together", "done", "use", "must", "day", "россии", "sure", "saying", "really", "trying", "every", "place", "may", "working", "help", "put", "around", "got", "которые", "continue", "kind", "fact", "never", "since", "ago", "fleischer", "albright", "another", "tell", "talk", "give", "thing", "able", "coming", "clear", "course", "forward", "days", "trade", "questions", "number", "ask",  "украины", "moscow", "whether", "yes", "gouse", "past", "making", "seen", "used", "week", "including", "three", "took", "find", "different", "process", "obviously", "bring", "future", "всё", "hard", "might", "next", "possible", "meet", "prime", "talking", "anything", "happen", "strong", "money", "september", "general", "karl", "answer", "getting", "taken", "months", "taking", "big", "try", "opportunity", "department", "rubio", "always", "asked", "heard", "understand", "matter", "agree", "lavrov", "already", "happened", "talked")

# Tokenizing and Initital Cleaning

clean_and_tokenize <- function(df, extra_stopwords = character()) {
  all_stopwords <- unique(c(stopwords("en"), stopwords("ru"), extra_stopwords))
  df %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% all_stopwords) %>%
    filter(str_detect(word, "^[a-zа-яё']+$"))  # Keep words in English or Russian
}

# Appling cleaning

cleaned_tokens <- clean_and_tokenize(df, custom_stopwords)

# Preview top 50 frequent words

cleaned_tokens %>% count(word, sort = TRUE) %>% print(n = 50)

# Removing unwanted (non-informative or noisy) tokens

clean_and_tokenize <- function(df, extra_stopwords = character()) {
  custom_stopwords <- unique(c(stopwords("en"), stopwords("ru"), extra_stopwords))
  
  df %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% custom_stopwords) %>%
    filter(str_detect(word, "^[a-zа-яё']+$")) %>%
    select(speaker, date, title, url, country, conflict, word)
}

# Defining unwanted words
unwanted_words <- c(
  "Lockhart", "lockhart", "gumbel", "atlantik", "парашютно", "vance", "slovakia", "digital", "провел", "магомед", "гудермесе", "докладывать", "доложено", "жителя", "максимальное", "произошедших", "russert", "advertisement","полка", "january", "content", "berger", "сергей", "rice", "germany", "remarks", "заместителю", "провести", "церемонии", "владимиру", "поручил", "путину", "трех", "человека", "прав", "й", "шос", "ташухаджиевых", "цагараев", "woodruff", "yeltsin", "gonna", "чечни", "июня", "ll", "bryant", "archived", "местных", "оказать", "netherlands", "aspen", "bruecke", "cet", "marshall", "грозный", "statedept", "курской", "helsinki", "blinken", "путин", "прибыл", "cards", "sergey", "gov", "websites", "broker", "совещание", "thirty", "семьи", "carlson", "tucker", "schweid", "президента", "кавказе", "северном", "eventsandtravel", "получили", "media", "alright", "eet", "homepage", "youtube", "председатель", "федеральной", "epstein", "выразил", "благодарность", "душанбе", "кардинальные", "комплексное", "комплексные", "окончании", "остановку", "произошли", "северокавказских", "dr", "владимир", "результате", "t", "вручил", "сайди", "цагараева", "современных", "villepin", "coordinated", "loudly", "streamed", "israelis", "austin", "wired", "s", "zelenskyy", "georgian", "федерации", "aartsen", "van", "андрей", "белоусов", "разделе", "чечне", "city", "афганистана", "совета", "тысяч", "председателя", "republic", "spokesperson", "finnish", "hall",  "highlight", "registered", "президент", "ходе", "моздоке", "подчеркнул",  "принимал", "словам", "экономического", "области", "greece", "internet",  "gedda", "priebus", "семье", "shaw",
  "bulgaria", "thankful", "biden",  "caucasus", "obama", "designation", "joe", "дел", "используют",  "аэропорту", "бюджетной", "виктору", "властями", "журналистам",  "задолженности", "запланированное", "зарплате", "касьянову", "командным", "машиной", "михаилу", "намерен", "пилота", "полета", "прекративших", "премьерами", "работникам", "селекторное", "су", "управлял", "христенко", "благородную", "брошенный", "выводится", "выполнили", "командир",  "кострому", "лейтенант", "майоров", "месту", "understandings", "groups", "aligned", "maskhadov", "десантно", "постоянными", "вдв", "иванов", "володин", "вайно", "антон", "speech", "inctitute", "est", "notice", "j", "german", "function", "antony", "monitoring", "экологии", "десантного", "природоохранной", "дмитрий", "директор", "валентина", "бортников", "institute", "viewing", "thursday", "press", "office", "online", "fund", "archive", "полк", "медведев", "матвиенко", "лавров", "колокольцев","дополнительные", "updated", "berlin", "tags", "https", "host", "directories", "bureaus", "please", "information", "chilly", "offices", "links", "administrative", "site", "deliver", "edit", "нарышкин", "мишустин", "михаил", "министр", "вячеслав", "александр", "транспорта", "деятельности", "членами", "представитель", "внешней", "contact", "live", "official", "hosted", "пострадавшим", "вывод","содействие","правоохранительного","российской","думы","внутренних","службы","current", "channel","coverage","released","mission","partnership","secure","promises", "вывода", "государственной", "вопросам", "администрации", "бортникову", "белоусову", "андрею", "areas", "edt", "обсудим", "исполняется", "директору", "взаимных", "васильевичу", "co", "pre", "рэмовичу", "рэмовичминистр", "приступим", "исполняются","терактов","гражданскими","амнистии","истребителе","ранения","расследования","вопрос","правительства","иностранных","оборонны","sensitive","history","policy","relations","durable","american","leadership", "новости", "забудут", "взвода", "важный", "борисовичспециальный", "александру", "friday", "travel", "affairs", "видеосвязи", "викторовичпредседатель", "june", "continued", "комментарии", "эдуардовичруководитель",
  "supported", "анатольевичзаместитель", "александровичминистр", "васильевичдиректор", "апреля", "владимировичпредседатель", "викторовичминистр", "евгеньевичдиректор", "ивановнапредседатель", "митинг", "направляясь", "вызов", "герой", "десантный", "обсуждению", "договоренности", "прекращением", "justice", "divide", "nato", "osce", "issues", "state", "territory", "strategic", "failure", "подчеркнута", "владимира", "путина", "жак", "ширак", "нато", "важность", "договорённости", "оперативное", "ударов", "mcclellan", "elect", "georgia", "venediktov", "lithuania", "georgia's", "blitzer", "siewert", "council", "polio", "liz", "truss", "tapper", "evan", "hudson", "monastyrskyy", "состоялся", "разговор", "подразделением", "посвященный", "роман", "событию", "заместитель", "провёл", "специальный", "lehrer", "clinton", "leavy", "turkey", "communist", "belie", "muir", "meyers", "pelley", "finland", "torture", "f", "stephanopoulos", "торжественной", "щетнев", "беседы", "обязанности", "самолета", "нужны", "меры", "украиной", "совещании", "nigeria", "bulgarian", "stoyanov", "pritzker", "ransomware", "boak", "kishida", "judy", "pritzker", "italy", "gcc", "дислокации", "звания", "ответили", "проходил", "catalog", "ua", "разделах", "emphasized", "operational", "verified", "giorgia", "meloni", "sweden", "kingdom", "ales", "bialiatski", "ханкале", "государства", "исполняющий", "руководитель", "задать", "приняли", "monitors", "yesterday's", "holding", "holodomor", "referenda", "passover", "nord", "scholz", "loans", "находился", "сообщил", "связанные", "совет", "уважаемые", "коллеги", "добрый", "greek", "sunday's", "enduring", "accountable", "cri", "republicans", "tonight", "героев", "дивизии", "николай", "слово", "поводу", "участие", "россией", "talbott", "range", "friends", "kishida's", "cia", "погибших", "полгода", "день", "хотел", "очень", "прежде", "face", "zakaria", "congratulate", "kyiv's",
  "первым", "го", "выступления", "стенограммы", "версия", "года", "bombed", "close", "challenges", "detention", "change", "allies", "объединенной", "сюда", "дата", "доступны", "лицензии", "материал", "impartial", "stream", "материалы", "опубликован", "официальный", "публикации", "thanksgiving", "сайт", "сайта", "ссылка", "текстовая", "conversation", "attribution", "http", "commons", "администрация", "ira", "news", "d", "creative", "ms", "lateral", "bi", "applause", "ways", "взрывов", "open", "год", "задачу", "open", "efforts", "tv", "geneva", "acts", "cycle", "федеральных", "постоянной", "базе", "принял", "главы", "madame", "assets", "events", "prague", "czech", "стал", "пришли", "правительство", "honest", "dutch", "daley", "classrooms", "resolution", "madeleine", "hide", "child", "seemed", "марта", "этому", "военной", "которое", "россия", "сил")

# Further cleaning already-tokenized text
 
remove_unwanted_words <- function(tokens, unwanted_words) {
  tokens %>% filter(!word %in% unwanted_words)
}

# TF-IDF calculation on cleaned speech content

calculate_tfidf <- function(tokens) {
  tokens %>%
    count(country, conflict, title, word, sort = TRUE) %>%
    bind_tf_idf(word, title, n)
  }
