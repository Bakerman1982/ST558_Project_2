
library(shiny)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(gapminder)
library(shinydashboard)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)
library(viridis)
library(ggridges)
library(writexl)

#Standard Shiny App server wrap
server <- function(input, output) {
  
  #############################
  ### URL Building Function ###
  #############################
  
  build_url <- function(base_url = "https://aviationweather.gov/api/data/",
                        endpoint = "metar",
                        icaoIDs = if (!is.null(input$icaoID) && input$icaoID != "") input$icaoID else "#US",
                        hours = if (!is.null(input$hours) && input$hours != "") as.numeric(input$hours) else 24,
                        time = "valid") {
    
    ###BEGIN: Collection of embedded helper functions and vectors used to validate inputs
    # Helper vector for verifying proper use of "@" + state_abbreviations formatting
    valid_states <- paste0("@", c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                                  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                                  "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                                  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
    
    # Helper set of vectors for containing valid icaoIDs.  I had to break this up into four batches of 504 icaoIDs of contiguous US airports and a 5th batch of icaoIDs for Alaska and Hawaii because of the limitations of characters per row in R.  
    valid_icaoID_1 <- c("KAAA", "KAAF", "KAAO", "KAAS", "KAAT", "KABE", "KABI", "KABQ", "KABR", "KABY", "KACB", "KACJ", "KACK", "KACP", "KACQ", "KACT", "KACV", "KACY", "KACZ", "KADC", "KADF", "KADG", "KADH", "KADM", "KADS", "KADT", "KADU", "KADW", "KAEG", "KAEJ", "KAEL", "KAEX", "KAFF", "KAFJ", "KAFK", "KAFN", "KAFO", "KAFP", "KAFW", "KAGC", "KAGO", "KAGR", "KAGS", "KAGZ", "KAHC", "KAHH", "KAHN", "KAHQ", "KAIA", "KAIB", "KAID", "KAIG", "KAIK", "KAIO", "KAIT", "KAIV", "KAIY", "KAIZ", "KAJG", "KAJO", "KAJR", "KAJZ", "KAKH", "KAKO", "KAKQ", "KAKR", "KALB", "KALI", "KALK", "KALM", "KALN", "KALO", "KALS", "KALW", "KALX", "KAMA", "KAMG", "KAMN", "KAMT", "KAMW", "KANB", "KAND", "KANE", "KANJ", "KANK", "KANP", "KANQ", "KANW", "KANY", "KAOC", "KAOH", "KAOO", "KAOV", "KAPA", "KAPC", "KAPF", "KAPG", "KAPH", "KAPN", "KAPT", "KAPV", "KAQO", "KAQP", "KAQR", "KAQW", "KARA", "KARB", "KARG", "KARM", "KARR", "KART", "KARV", "KARW", "KASD", "KASE", "KASG", "KASH", "KASJ", "KASL", "KASN", "KAST", "KASW", "KASX", "KASY", "KATA", "KATL", "KATS", "KATW", "KATY", "KAUG", "KAUH", "KAUM", "KAUN", "KAUO", "KAUS", "KAUW", "KAVC", "KAVK", "KAVL", "KAVO", "KAVP", "KAVQ", "KAVX", "KAWG", "KAWM", "KAWO", "KAXA", "KAXH", "KAXN", "KAXQ", "KAXS", "KAXV", "KAXX", "KAYS", "KAYX", "KAZC", "KAZE", "KAZO", "KAZU", "KBAB", "KBAC", "KBAD", "KBAF", "KBAK", "KBAM", "KBAN", "KBAX", "KBAZ", "KBBB", "KBBD", "KBBG", "KBBP", "KBBW", "KBCB", "KBCE", "KBCK", "KBCT", "KBDE", "KBDG", "KBDH", "KBDJ", "KBDL", "KBDN", "KBDQ", "KBDR", "KBDU", "KBEC", "KBED", "KBEH", "KBFA", "KBFD", "KBFE", "KBFF", "KBFI", "KBFK", "KBFL", "KBFM", "KBFR", "KBFW", "KBGD", "KBGE", "KBGF", "KBGM", "KBGR", "KBHB", "KBHC", "KBHK", "KBHM", "KBID", "KBIE", "KBIF", "KBIH", "KBIL", "KBIS", "KBIV", "KBIX", "KBJC", "KBJI", "KBJJ", "KBJN", "KBKD", "KBKE", "KBKF", "KBKL", "KBKN", "KBKS", "KBKT", "KBKV", "KBKW", "KBKX", "KBLF", "KBLH", "KBLI", "KBLM", "KBLU", "KBLV", "KBMC", "KBMG", "KBMI", "KBML", "KBMQ", "KBMT", "KBNA", "KBNG", "KBNL", "KBNO", "KBNW", "KBOI", "KBOK", "KBOS", "KBOW", "KBPG", "KBPI", "KBPK", "KBPP", "KBPT", "KBQK", "KBQR", "KBRD", "KBRL", "KBRO", "KBRY", "KBST", "KBTA", "KBTF", "KBTL", "KBTM", "KBTN", "KBTP", "KBTR", "KBTV", "KBTY", "KBUB", "KBUF", "KBUM", "KBUR", "KBUU", "KBUY", "KBVI", "KBVN", "KBVO", "KBVS", "KBVU", "KBVX", "KBVY", "KBWC", "KBWD", "KBWG", "KBWI", "KBWP", "KBXA", "KBXG", "KBXK", "KBXM", "KBYG", "KBYH", "KBYI", "KBYS", "KBYY", "KBZN", "KCAD", "KCAE", "KCAG", "KCAK", "KCAO", "KCAR", "KCAV", "KCBE", "KCBF", "KCBG", "KCBK", "KCBM", "KCCA", "KCCB", "KCCO", "KCCR", "KCCY", "KCDA", "KCDC", "KCDD", "KCDH", "KCDI", "KCDK", "KCDN", "KCDR", "KCDS", "KCDW", "KCEA", "KCEC", "KCEF", "KCEK", "KCEU", "KCEV", "KCEW", "KCEY", "KCEZ", "KCFD", "KCFE", "KCFJ", "KCFS", "KCFT", "KCFV", "KCGC", "KCGE", "KCGF", "KCGI", "KCGS", "KCGX", "KCGZ", "KCHA", "KCHD", "KCHK", "KCHN", "KCHO", "KCHQ", "KCHS", "KCHT", "KCHU", "KCIC", "KCID", "KCII", "KCIN", "KCIR", "KCIU", "KCJJ", "KCJR", "KCKA", "KCKB", "KCKC", "KCKF", "KCKI", "KCKM", "KCKN", "KCKP", "KCKV", "KCLE", "KCLI", "KCLK", "KCLL", "KCLM", "KCLR", "KCLS", "KCLT", "KCLW", "KCMA", "KCMH", "KCMI", "KCMR", "KCMX", "KCMY", "KCNC", "KCNH", "KCNI", "KCNK", "KCNM", "KCNO", "KCNP", "KCNU", "KCNW", "KCNY", "KCOD", "KCOE", "KCOF", "KCOI", "KCOM", "KCON", "KCOQ", "KCOS", "KCOT", "KCOU", "KCPC", "KCPF", "KCPK", "KCPM", "KCPR", "KCPS", "KCPT", "KCPU", "KCQA", "KCQB", "KCQC", "KCQF", "KCQM", "KCQW", "KCQX", "KCRE", "KCRG", "KCRO", "KCRP", "KCRQ", "KCRS", "KCRT", "KCRW", "KCRX", "KCRZ", "KCSB", "KCSG", "KCSM", "KCSQ", "KCSV", "KCTB", "KCTJ", "KCTK", "KCTY", "KCTZ", "KCUB", "KCUH", "KCUL", "KCUT", "KCVG", "KCVH", "KCVK", "KCVN", "KCVO", "KCVS", "KCVX", "KCWA", "KCWC", "KCWF", "KCWI", "KCWS", "KCWV", "KCXE", "KCXL", "KCXO", "KCXP", "KCXU", "KCXW", "KCXY", "KCYO", "KCYS", "KCYW", "KCZD", "KCZG", "KCZK", "KCZL", "KCZT", "KDAA", "KDAB", "KDAF", "KDAG", "KDAL", "KDAN", "KDAW", "KDAY", "KDBN", "KDBQ", "KDCA", "KDCU", "KDCY", "KDDC", "KDDH", "KDEC", "KDED", "KDEH", "KDEN", "KDEQ", "KDET", "KDEW", "KDFI", "KDFW", "KDGL", "KDGW", "KDHN");
    valid_icaoID_2 <- c("KDHT", "KDIJ", "KDIK", "KDKB", "KDKK", "KDKR", "KDKX", "KDLC", "KDLF", "KDLH", "KDLL", "KDLN", "KDLO", "KDLS", "KDLZ", "KDMA", "KDMN", "KDMO", "KDMW", "KDNA", "KDNL", "KDNN", "KDNS", "KDNV", "KDOV", "KDPA", "KDPG", "KDPL", "KDQH", "KDRA", "KDRI", "KDRO", "KDRT", "KDRU", "KDSM", "KDSV", "KDTA", "KDTG", "KDTL", "KDTN", "KDTO", "KDTS", "KDTW", "KDUA", "KDUC", "KDUG", "KDUH", "KDUJ", "KDUX", "KDVK", "KDVL", "KDVN", "KDVO", "KDVP", "KDVT", "KDWH", "KDWU", "KDXE", "KDXR", "KDXX", "KDYA", "KDYB", "KDYL", "KDYR", "KDYS", "KDYT", "KDZJ", "KEAG", "KEAN", "KEAR", "KEAT", "KEAU", "KEBG", "KEBS", "KECG", "KECP", "KECS", "KECU", "KEDC", "KEDE", "KEDG", "KEDJ", "KEDN", "KEDU", "KEDW", "KEED", "KEEN", "KEEO", "KEET", "KEFC", "KEFD", "KEFK", "KEFT", "KEFW", "KEGE", "KEGI", "KEGQ", "KEGT", "KEGV", "KEHA", "KEHO", "KEHR", "KEIK", "KEIW", "KEKA", "KEKM", "KEKN", "KEKO", "KEKQ", "KEKS", "KEKX", "KEKY", "KELA", "KELD", "KELK", "KELM", "KELN", "KELO", "KELP", "KELY", "KELZ", "KEMM", "KEMP", "KEMT", "KEMV", "KEND", "KENL", "KENV", "KENW", "KEOE", "KEOK", "KEOP", "KEOS", "KEPG", "KEPH", "KEPM", "KEQA", "KEQY", "KERI", "KERR", "KERV", "KERY", "KESC", "KESF", "KESN", "KEST", "KESW", "KETB", "KETC", "KETN", "KEUF", "KEUG", "KEUL", "KEVB", "KEVM", "KEVU", "KEVV", "KEVW", "KEVY", "KEWB", "KEWK", "KEWN", "KEWR", "KEXX", "KEYE", "KEYF", "KEYQ", "KEYW", "KEZF", "KEZI", "KEZM", "KEZS", "KEZZ", "KFAF", "KFAM", "KFAR", "KFAT", "KFAY", "KFBG", "KFBL", "KFBR", "KFBY", "KFCA", "KFCH", "KFCI", "KFCM", "KFCS", "KFCT", "KFCY", "KFDK", "KFDR", "KFDW", "KFDY", "KFEP", "KFES", "KFET", "KFFA", "KFFC", "KFFL", "KFFM", "KFFO", "KFFT", "KFFZ", "KFGU", "KFGX", "KFHB", "KFHR", "KFHU", "KFIG", "KFIT", "KFKA", "KFKL", "KFKN", "KFKR", "KFKS", "KFLD", "KFLG", "KFLL", "KFLO", "KFLP", "KFLV", "KFLX", "KFLY", "KFME", "KFMH", "KFMM", "KFMN", "KFMY", "KFMZ", "KFNB", "KFNL", "KFNT", "KFOA", "KFOD", "KFOE", "KFOK", "KFOM", "KFOT", "KFOZ", "KFPK", "KFPR", "KFQD", "KFRG", "KFRH", "KFRI", "KFRM", "KFRR", "KFSD", "KFSE", "KFSI", "KFSK", "KFSM", "KFSO", "KFST", "KFSU", "KFSW", "KFTG", "KFTK", "KFTT", "KFTW", "KFTY", "KFUL", "KFVE", "KFVX", "KFWA", "KFWC", "KFWN", "KFWQ", "KFWS", "KFXE", "KFXY", "KFYE", "KFYJ", "KFYM", "KFYV", "KFZG", "KFZI", "KFZY", "KGAB", "KGAD", "KGAF", "KGAG", "KGAI", "KGAO", "KGBD", "KGBR", "KGCC", "KGCD", "KGCK", "KGCM", "KGCN", "KGDJ", "KGDM", "KGDP", "KGDV", "KGDY", "KGED", "KGEG", "KGEU", "KGEV", "KGEY", "KGFA", "KGFK", "KGFL", "KGGE", "KGGF", "KGGG", "KGGI", "KGGW", "KGHG", "KGHM", "KGIC", "KGIF", "KGJT", "KGKJ", "KGKT", "KGKY", "KGLD", "KGLH", "KGLS", "KGLW", "KGMJ", "KGMU", "KGNB", "KGNC", "KGNF", "KGNG", "KGNI", "KGNT", "KGNV", "KGOK", "KGON", "KGOO", "KGOV", "KGPI", "KGPT", "KGPZ", "KGRB", "KGRD", "KGRF", "KGRI", "KGRK", "KGRN", "KGRR", "KGSB", "KGSO", "KGSP", "KGSW", "KGTB", "KGTE", "KGTF", "KGTG", "KGTR", "KGTU", "KGUC", "KGUP", "KGUS", "KGUY", "KGVE", "KGVQ", "KGVT", "KGWB", "KGWO", "KGWR", "KGWS", "KGWW", "KGXA", "KGXF", "KGXY", "KGYB", "KGYH", "KGYI", "KGYR", "KGYY", "KGZH", "KGZL", "KHAB", "KHAE", "KHAF", "KHAI", "KHAO", "KHAR", "KHBC", "KHBG", "KHBI", "KHBR", "KHCR", "KHBZ", "KHCD", "KHDE", "KHDN", "KHDO", "KHEE", "KHEF", "KHEG", "KHEI", "KHEQ", "KHEY", "KHEZ", "KHFD", "KHFF", "KHFJ", "KHGR", "KHHF", "KHHR", "KHHW", "KHIB", "KHIE", "KHIF", "KHII", "KHIO", "KHJH", "KHJO", "KHKA", "KHKS", "KHKY", "KHLC", "KHLG", "KHLN", "KHLR", "KHLX", "KHMN", "KHMS", "KHMT", "KHMZ", "KHND", "KHNZ", "KHOB", "KHOE", "KHON", "KHOP", "KHOT", "KHOU", "KHPN", "KHQG", "KHQM", "KHQU", "KHQZ", "KHRI", "KHRJ", "KHRL", "KHRO", "KHRT", "KHRU", "KHRX", "KHSA", "KHSE", "KHSI", "KHSP", "KHSR", "KHST", "KHSV", "KHTH", "KHTO", "KHTS", "KHUA", "KHUF", "KHUL", "KHUM", "KHUT", "KHVC", "KHVE", "KHVN", "KHVR", "KHVS", "KHWD", "KHWO", "KHWQ", "KHWV", "KHWY", "KHXD", "KHXF", "KHYA", "KHYI", "KHYR", "KHYS", "KHYW", "KHYX", "KHZE", "KHZL", "KIAB", "KIAD", "KIAG", "KIAH", "KIBM", "KICR", "KICT", "KIDA", "KIDI", "KIDL", "KIDP", "KIEN", "KIFP", "KIGM", "KIGX", "KIIB", "KIIY", "KIJD", "KIJX", "KIKV", "KIKW", "KILE", "KILG", "KILM");
    valid_icaoID_3 <- c("KILN", "KIML", "KIMM", "KIMS", "KIMT", "KIND", "KINJ", "KINK", "KINL", "KINS", "KINT", "KINW", "KIOW", "KIPJ", "KIPL", "KIPT", "KIRK", "KISM", "KISN", "KISO", "KISP", "KISW", "KITH", "KITR", "KIWA", "KIWI", "KIWD", "KIXD", "KIYK", "KIZA", "KIZG", "KJAC", "KJAN", "KJAX", "KJBR", "KJCT", "KJDN", "KJEF", "KJFK", "KJFX", "KJER", "KJGG", "KJHN", "KJHW", "KJKA", "KJLN", "KJMS", "KJNX", "KJQF", "KJRA", "KJRB", "KJST", "KJSV", "KJVW", "KJWG", "KJWN", "KJYO", "KJYR", "KJZI", "KJZP", "KKIC", "KKLS", "KKNB", "KKY8", "KLAA", "KLAF", "KLAL", "KLAM", "KLAN", "KLAR", "KLAS", "KLAW", "KLAX", "KLBB", "KLBE", "KLBF", "KLBL", "KLBR", "KLBT", "KLBX", "KLCG", "KLCH", "KLCI", "KLCK", "KLCQ", "KLDJ", "KLDM", "KLEB", "KLEE", "KLEM", "KLEW", "KLEX", "KLFI", "KLFK", "KLFT", "KLGA", "KLGB", "KLGD", "KLGF", "KLGU", "KLHB", "KLHM", "KLHQ", "KLHV", "KLHW", "KLHX", "KLHZ", "KLIC", "KLIT", "KLIZ", "KLKP", "KLKR", "KLKU", "KLKV", "KLLJ", "KLLQ", "KLLU", "KLMO", "KLMS", "KLMT", "KLNA", "KLNC", "KLND", "KLNK", "KLNN", "KLNP", "KLNS", "KLOL", "KLOR", "KLOT", "KLOU", "KLOZ", "KLPC", "KLPR", "KLQK", "KLQR", "KLRD", "KLRF", "KLRG", "KLRU", "KLSB", "KLSE", "KLSF", "KLSK", "KLSN", "KLSV", "KLTS", "KLTY", "KLUF", "KLUG", "KLUK", "KLUL", "KLVK", "KLVL", "KLVM", "KLVN", "KLVS", "KLWB", "KLWC", "KLWL", "KLWM", "KLWS", "KLWT", "KLWV", "KLXL", "KLXN", "KLXT", "KLXV", "KLYH", "KLYO", "KLZU", "KLZZ", "KMAC", "KMAE", "KMAF", "KMAI", "KMAL", "KMAN", "KMAO", "KMAW", "KMBG", "KMBO", "KMBS", "KMBT", "KMCB", "KMCC", "KMCE", "KMCF", "KMCI", "KMCK", "KMCN", "KMCO", "KMCW", "KMCZ", "KMDD", "KMDQ", "KMDS", "KMDT", "KMDW", "KMDZ", "KMEB", "KMEI", "KMEJ", "KMEM", "KMER", "KMEV", "KMFE", "KMFI", "KMFR", "KMFV", "KMGE", "KMGG", "KMGJ", "KMGM", "KMGW", "KMGY", "KMHE", "KMHK", "KMHL", "KMHN", "KMHR", "KMHS", "KMHT", "KMHV", "KMIA", "KMIB", "KMIC", "KMIO", "KMIT", "KMIV", "KMJX", "KMKA", "KMKC", "KMKE", "KMKG", "KMKJ", "KMKL", "KMKO", "KMKT", "KMKY", "KMLB", "KMLC", "KMLD", "KMLE", "KMLF", "KMLI", "KMLS", "KMLT", "KMLU", "KMMH", "KMMI", "KMMK", "KMML", "KMMS", "KMMT", "KMMU", "KMMV", "KMNI", "KMNZ", "KMOB", "KMOD", "KMOR", "KMOT", "KMPE", "KMPI", "KMPJ", "KMPO", "KMPR", "KMPV", "KMQI", "KMQJ", "KMQS", "KMQY", "KMRB", "KMRF", "KMRH", "KMRN", "KMRY", "KMSL", "KMSN", "KMSO", "KMSP", "KMSS", "KMSV", "KMSY", "KMTC", "KMTH", "KMTJ", "KMTN", "KMTP", "KMTV", "KMTW", "KMUO", "KMUT", "KMUU", "KMVC", "KMVI", "KMVL", "KMVM", "KMVY", "KMWH", "KMWC", "KMWK", "KMWL", "KMXA", "KMXF", "KMXO", "KMYF", "KMYL", "KMYR", "KMYV", "KMYZ", "KMZJ", "KNAB", "KNBC", "KNBG", "KNBJ", "KNBW", "KNCA", "KNDY", "KNDZ", "KNEL", "KNEN", "KNEW", "KNFD", "KNFE", "KNFG", "KNFJ", "KNFL", "KNFW", "KNGP", "KNGS", "KNGU", "KNGZ", "KNHK", "KNHL", "KNHZ", "KNID", "KNIP", "KNJK", "KNJM", "KNJW", "KNKL", "KNKT", "KNKX", "KNLC", "KNMM", "KNOW", "KNPA", "KNPI", "KNQA", "KNQB", "KNQX", "KNRA", "KNRB", "KNRN", "KNRQ", "KNRS", "KNSE", "KNSI", "KNTD", "KNTK", "KNTU", "KNUC", "KNUI", "KNUN", "KNUQ", "KNUW", "KNVI", "KNWL", "KNYG", "KNYL", "KNXF", "KNXP", "KNXX", "KNZJ", "KNZY", "KOAJ", "KOAK", "KOAR", "KOBE", "KOBI", "KOCF", "KOCH", "KOCW", "KODO", "KODX", "KOEL", "KOFF", "KOFK", "KOFP", "KOGA", "KOGB", "KOGD", "KOGS", "KOIC", "KOIN", "KOJA", "KOJC", "KOKB", "KOKC", "KOKK", "KOKM", "KOKS", "KOKV", "KOLD", "KOLE", "KOLF", "KOLM", "KOLS", "KOLU", "KOLV", "KOLZ", "KOMA", "KOMH", "KOMK", "KOMN", "KONA", "KONL", "KONM", "KONO", "KONP", "KONT", "KONX", "KOPF", "KOPL", "KOQN", "KOQU", "KORB", "KORD", "KORE", "KORF", "KORG", "KORH", "KORL", "KORS", "KOSC", "KOSH", "KOSU", "KOSX", "KOTH", "KOTM", "KOUN", "KOVE", "KOVS", "KOWB", "KOWD", "KOWI", "KOWK", "KOXB", "KOXC", "KOXD", "KOXR", "KOYM", "KOZA", "KOZR", "KOZS", "KOZW", "KPAE", "KPAH", "KPAM", "KPAN", "KPAO", "KPBF", "KPBG", "KPBI", "KPBX", "KPCM", "KPCU", "KPCW", "KPCZ", "KPDC", "KPDK", "KPDT", "KPDX", "KPEO", "KPEQ", "KPFC", "KPFN", "KPGA", "KPGD", "KPGR", "KPGV", "KPHD", "KPHF", "KPHG", "KPHH", "KPHK", "KPHL", "KPHP", "KPHT", "KPHX", "KPIA", "KPIB", "KPIE", "KPIH", "KPIR", "KPIT", "KPKB", "KPKV", "KPLB", "KPLK", "KPLN", "KPLR");
    valid_icaoID_4 <- c("KPLU", "KPMB", "KPMD", "KPMV", "KPMZ", "KPNA", "KPNC", "KPNE", "KPNM", "KPNN", "KPNS", "KPOB", "KPOC", "KPOU", "KPOY", "KPPA", "KPPF", "KPQI", "KPQL", "KPRB", "KPRC", "KPRN", "KPRX", "KPSB", "KPSC", "KPSF", "KPSK", "KPSM", "KPSN", "KPSO", "KPSP", "KPTB", "KPTD", "KPTK", "KPTN", "KPTS", "KPTT", "KPTV", "KPTW", "KPUB", "KPUC", "KPUJ", "KPUW", "KPVB", "KPVC", "KPVD", "KPVE", "KPVF", "KPVG", "KPVJ", "KPVU", "KPVW", "KPWA", "KPWD", "KPWK", "KPWM", "KPWT", "KPXE", "KPYG", "KPYM", "KPYP", "KPYX", "KQA7", "KQAD", "KQAE", "KQAJ", "KQAO", "KQAQ", "KQAX", "KQAY", "KQCO", "KQCT", "KQCU", "KQD9", "KQDM", "KQEZ", "KQGV", "KQGX", "KQIR", "KQIU", "KQL5", "KQMA", "KQMG", "KQMH", "KQNC", "KQNN", "KQNS", "KQNT", "KQNY", "KQOS", "KQPC", "KQPD", "KQRY", "KQSA", "KQSE", "KQSL", "KQSM", "KQSR", "KQTA", "KQTI", "KQTO", "KQTU", "KQTX", "KQTZ", "KQVO", "KQWM", "KQXJ", "KQXN", "KQYB", "KRAC", "KRAL", "KRAP", "KRAW", "KRBD", "KRBE", "KRBG", "KRBL", "KRBM", "KRBW", "KRCA", "KRCE", "KRCM", "KRCT", "KRDD", "KRDG", "KRDM", "KRDR", "KRDU", "KRED", "KREG", "KREI", "KREO", "KRFD", "KRGK", "KRHI", "KRHP", "KRHV", "KRIC", "KRID", "KRIF", "KRIL", "KRIR", "KRIU", "KRIV", "KRIW", "KRJD", "KRKR", "KRKD", "KRKS", "KRLD", "KRME", "KRMN", "KRND", "KRNM", "KRNO", "KRNT", "KRNV", "KROA", "KROC", "KROG", "KROW", "KRPB", "KRPD", "KRPH", "KRPX", "KRQE", "KRRT", "KRSL", "KRST", "KRSW", "KRTN", "KRTS", "KRUE", "KRUG", "KRUQ", "KRUT", "KRVL", "KRVS", "KRWI", "KRWL", "KRWV", "KRXE", "KRYN", "KRYV", "KRYW", "KRYY", "KRZL", "KRZN", "KRZT", "KRZZ", "KSAA", "KSAC", "KSAD", "KSAF", "KSAN", "KSAS", "KSAT", "KSAV", "KSAW", "KSAZ", "KSBA", "KSBD", "KSBM", "KSBN", "KSBO", "KSBP", "KSBS", "KSBX", "KSBY", "KSCB", "KSCD", "KSCH", "KSCK", "KSCR", "KSDC", "KSDF", "KSDL", "KSDM", "KSDY", "KSEA", "KSEE", "KSEF", "KSEG", "KSEM", "KSEP", "KSEZ", "KSFB", "KSFD", "KSFF", "KSFM", "KSFO", "KSFQ", "KSFZ", "KSGF", "KSGJ", "KSGT", "KSGU", "KSHD", "KSHN", "KSHR", "KSHV", "KSIF", "KSIK", "KSIY", "KSJC", "KSJN", "KSJT", "KSKA", "KSKF", "KSKI", "KSKX", "KSLB", "KSLC", "KSLE", "KSLG", "KSLI", "KSLK", "KSLN", "KSLR", "KSMD", "KSME", "KSMF", "KSMN", "KSMO", "KSMQ", "KSMS", "KSMX", "KSNA", "KSNC", "KSNK", "KSNL", "KSNS", "KSNT", "KSNY", "KSOA", "KSOP", "KSOW", "KSPA", "KSPB", "KSPD", "KSPF", "KSPG", "KSPH", "KSPI", "KSPS", "KSPW", "KSPX", "KSPZ", "KSQL", "KSRC", "KSRQ", "KSRR", "KSSC", "KSSF", "KSSI", "KSSN", "KSSQ", "KSTC", "KSTF", "KSTK", "KSTL", "KSTP", "KSTS", "KSUA", "KSUN", "KSUS", "KSUT", "KSUU", "KSUW", "KSUX", "KSUZ", "KSVC", "KSVE", "KSVH", "KSWF", "KSWI", "KSWO", "KSWT", "KSWW", "KSXL", "KSXT", "KSXU", "KSYF", "KSYI", "KSYL", "KSYN", "KSYR", "KSZL", "KSZP", "KSZT", "KTAD", "KTAN", "KTBN", "KTBR", "KTBX", "KTCC", "KTCL", "KTCM", "KTCS", "KTCY", "KTDF", "KTDO", "KTDW", "KTEB", "KTEL", "KTEX", "KTGI", "KTHM", "KTHP", "KTHV", "KTIK", "KTIW", "KTIX", "KTKI", "KTKO", "KTKV", "KTLH", "KTLR", "KTMB", "KTME", "KTMK", "KTNP", "KTNT", "KTNU", "KTNX", "KTOA", "KTOI", "KTOL", "KTOP", "KTOR", "KTPA", "KTPF", "KTPH", "KTPL", "KTQE", "KTQH", "KTQK", "KTRI", "KTRK", "KTRM", "KTRX", "KTSP", "KTTA", "KTTD", "KTTF", "KTTN", "KTUL", "KTUP", "KTUS", "KTVC", "KTVF", "KTVL", "KTVR", "KTVY", "KTVZ", "KTWF", "KTWT", "KTXK", "KTXW", "KTYL", "KTYQ", "KTYR", "KTYS", "KTZR", "KTZT", "KUAO", "KUBE", "KUBS", "KUCA", "KUCP", "KUDD", "KUDG", "KUES", "KUGN", "KUIL", "KUIN", "KUKF", "KUKI", "KUKL", "KUKT", "KULS", "KUMP", "KUNI", "KUNU", "KUNV", "KUOS", "KUOX", "KUTA", "KUTS", "KUUU", "KUVA", "KUZA", "KVAY", "KVBG", "KVBT", "KVBW", "KVCB", "KVCT", "KVCV", "KVDF", "KVEL", "KVER", "KVES", "KVGT", "KVIH", "KVIS", "KVJI", "KVKS", "KVKX", "KVLD", "KVLL", "KVMR", "KVNC", "KVNY", "KVPS", "KVPZ", "KVQQ", "KVRB", "KVSF", "KVTA", "KVTN", "KVUJ", "KVUO", "KVVS", "KVYS", "KWAL", "KWAY", "KWBW", "KWDG", "KWDR", "KWHP", "KWJF", "KWLD", "KWLW", "KWMC", "KWRB", "KWRI", "KWRL", "KWST", "KWVI", "KWVL", "KWWD", "KWWR", "KWYS", "KXBP", "KXFL", "KXLL", "KXMR", "KXNA", "KXNO", "KXNX", "KXTA", "KXVG", "KXWA", "KYIP", "KYKM", "KYKN", "KYNG", "KZEF", "KZER", "KZPH", "KZUN", "KZZV");
    valid_icaoID_5 <- c("PAAK", "PAAL", "PAAM", "PAAN", "PAAP", "PAAQ", "PAAT", "PABA", "PABE", "PABG", "PABI", "PABL", "PABM", "PABN", "PABR", "PABT", "PABU", "PABV", "PACA", "PACD", "PACE", "PACH", "PACI", "PACJ", "PACK", "PACL", "PACM", "PACR", "PACS", "PACV", "PACX", "PACZ", "PADE", "PADG", "PADK", "PADL", "PADM", "PADQ", "PADT", "PADU", "PADY", "PAED", "PAEE", "PAEG", "PAEH", "PAEI", "PAEL", "PAEM", "PAEN", "PAEW", "PAFA", "PAFB", "PAFE", "PAFK", "PAFL", "PAFM", "PAFR", "PAFS", "PAFV", "PAGA", "PAGB", "PAGG", "PAGH", "PAGK", "PAGL", "PAGM", "PAGN", "PAGQ", "PAGS", "PAGT", "PAGX", "PAGY", "PAHC", "PAHL", "PAHN", "PAHO", "PAHP", "PAHU", "PAHV", "PAHX", "PAHY", "PAIG", "PAII", "PAIK", "PAIL", "PAIM", "PAIN", "PAIW", "PAJC", "PAJN", "PAJO", "PAJV", "PAJZ", "PAKA", "PAKD", "PAKF", "PAKH", "PAKI", "PAKK", "PAKL", "PAKN", "PAKO", "PAKP", "PAKT", "PAKU", "PAKV", "PAKW", "PAKY", "PALB", "PALG", "PALH", "PALJ", "PALN", "PALP", "PALR", "PALU", "PAMB", "PAMC", "PAMD", "PAMH", "PAMK", "PAML", "PAMM", "PAMO", "PAMR", "PAMX", "PAMY", "PANA", "PANC", "PANI", "PANN", "PANO", "PANR", "PANT", "PANU", "PANV", "PANW", "PAOB", "PAOC", "PAOH", "PAOM", "PAOO", "PAOR", "PAOT", "PAOU", "PAPB", "PAPC", "PAPE", "PAPG", "PAPH", "PAPK", "PAPM", "PAPN", "PAPO", "PAPR", "PAQC", "PAQH", "PAQT", "PARC", "PARS", "PARY", "PASA", "PASC", "PASD", "PASH", "PASI", "PASK", "PASL", "PASM", "PASN", "PASO", "PASP", "PAST", "PASV", "PASW", "PASX", "PASY", "PATA", "PATC", "PATE", "PATG", "PATJ", "PATK", "PATL", "PATQ", "PATW", "PAUK", "PAUM", "PAUN", "PAUO", "PAUT", "PAVA", "PAVC", "PAVD", "PAVE", "PAVL", "PAWB", "PAWD", "PAWG", "PAWI", "PAWM", "PAWN", "PAWR", "PAWS", "PAWT", "PAXK", "PAYA", "PFAK", "PFAL", "PFCB", "PFCL", "PFEL", "PFKA", "PFKK", "PFKO", "PFKT", "PFKU", "PFKW", "PFMP", "PFNO", "PFSH", "PFTO", "PFWS", "PFYU", "POLI", "PPDM", "PPIT", "PPIZ", "PHBK", "PHDH", "PHHF", "PHHI", "PHHN", "PHIK", "PHJH", "PHJR", "PHKO", "PHLI", "PHLU", "PHMK", "PHMU", "PHNG", "PHNL", "PHNP", "PHNY", "PHOG", "PHPA", "PHSF", "PHTO", "PHUP")
    
    # Helper function to check ICAO IDs format
    validate_icaoIDs <- function(icaoIDs) {
      valid_special_ids <- c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW", "#US")
      valid_state <- icaoIDs %in% valid_states
      valid_special <- icaoIDs %in% valid_special_ids
      valid_icao_single <- grepl("^[KP][A-Z]{3}$", icaoIDs, ignore.case = TRUE)
      valid_icao_multiple <- grepl("^([KP][A-Z]{3}[ ,]?)+$", icaoIDs, ignore.case = TRUE)
      return(valid_state || valid_special || valid_icao_single || valid_icao_multiple)
    }
    
    # Helper function to check if ICAO ID is in valid ICAO ID vectors
    check_icao_in_valid_vectors <- function(icaoID) {
      all_valid_icaoIDs <- c(valid_icaoID_1, valid_icaoID_2, valid_icaoID_3, valid_icaoID_4, valid_icaoID_5)
      return(icaoID %in% all_valid_icaoIDs)
    } #END: Helper Section
    
    
    
    ###BEGIN: Input Validation
    #EndPoints
    # Defined endpoints
    valid_endpoints <- c("metar", "taf", "airport")
    
    # Convert endpoint to lowercase for internal processing
    endpoint_lower <- tolower(endpoint)
    
    # logic to test if provided endpoint input is valid
    if (!(endpoint_lower %in% valid_endpoints)) {
      stop("Invalid endpoint. Choose from: ", paste(valid_endpoints, collapse = ", "))
    }
    
    
    #icaoIDs        
    # Define different pattern cases for acceptable icaoID inputs
    if (!is.null(icaoIDs)) {
      if (!(grepl("^@[A-Z]{2}$", icaoIDs, ignore.case = TRUE) || 
            grepl("^@[A-Z]{4}$", icaoIDs, ignore.case = TRUE) || 
            grepl("^@?(?:[A-Z]{4}[ ,]?)+$", icaoIDs, ignore.case = TRUE) || 
            icaoIDs %in% c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW", "#US") || 
            icaoIDs %in% paste0("@", valid_states))) {
        stop("Invalid icaoIDs format. Must be a capitalized two-letter state abbreviation prefixed by '@', a four-letter ICAO ID, multiple four-letter ICAO IDs separated by commas or spaces, or a special identifier (@TOP, @TOPE, @TOPC, @TOPW, @USN, @USE, @USS, @USW, #US).  See `Valid icaoIDs` tab for acceptable variants.")
      }
      if (endpoint_lower == "taf" && icaoIDs == "#US") {
        stop("icaoID = '#US' is not allowed for endpoint 'taf'.")
      }
    } else if (endpoint_lower %in% c("taf", "airport")) {
      stop("icaoIDs parameter is required for endpoints 'TAF' and 'airport'.")
    }
    
    
    #Hours
    # Validate Input Hours when `metar`
    if (endpoint_lower == "metar" && !is.null(hours)) {
      if (!(is.numeric(hours) && hours == as.integer(hours) && hours > 0)) {
        stop("Invalid hours. Must be a positive whole number.")
      }
    }
    
    
    #Time          
    # Validate Input Time when `taf`
    if (endpoint_lower == "taf" && !is.null(time)) {
      if (!(time %in% c("issue", "valid"))) {
        stop("Invalid time. Must be 'issue' or 'valid'.")
      }
    }#END: Input Validation Section
    
    
    
    ###BEGIN: URL Contruction
    # Create a placeholder list to ingest the query parameters
    params <- list()
    
    
    # This `if` statement makes sure the icaoID patterns are properly held for URL building.  Different endpoints require different URL formatting
    if (!is.null(icaoIDs)) {
      if (endpoint_lower %in% c("metar", "taf", "airport")) {
        if (icaoIDs %in% c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW") || icaoIDs %in% valid_states) {
          params$ids <- paste0(icaoIDs)  # Keep special identifiers as-is with '@'
        } else if (icaoIDs == "#US" && endpoint_lower != "taf") {
          params$ids <- gsub("#", "%23",icaoIDs)  # Keep #US as-is for endpoints other than 'taf'
        } else if (grepl("^[A-Z]{4}$", icaoIDs, ignore.case = TRUE)) {
          params$ids <- icaoIDs
        } else if (grepl(",", icaoIDs)) {# Allow comma-separated ids
          params$ids <- gsub(" ", "%2C", icaoIDs)
        } else {
          stop("Invalid icaoIDs format. Must be a capitalized two-letter state abbreviation prefixed by '@', a four-letter ICAO ID, multiple four-letter ICAO IDs separated by commas, or a special identifier (@TOP, @TOPE, @TOPC, @TOPW, @USN, @USE, @USS, @USW, #US).  See `Valid icaoIDs` tab for acceptable variants.")
        }
      }
    }
    
    
    # Add specific parameters based on the endpoint
    if (endpoint_lower == "metar" && !is.null(hours)) {
      params$hours <- hours
    } else if (endpoint_lower == "taf" && !is.null(time)) {
      params$time <- time
    }
    
    
    # Add format=json as the default parameter
    params$format <- "json"
    
    
    # Construct the complete URL with query parameters using the parameterization formatting above.  Five cases.
    if (endpoint_lower == "metar") {
      if (is.null(hours)) {
        complete_url <- paste0(base_url, "metar?", "ids=", params$ids, "&format=json")
      } else {
        complete_url <- paste0(base_url, "metar?", "ids=", params$ids, "&format=json", "&hours=", hours)
      }
    } else if (endpoint_lower == "taf") {
      if (is.null(time)) {
        complete_url <- paste0(base_url, "taf?", "ids=", params$ids,"&format=json")
      } else {
        complete_url <- paste0(base_url, "taf?", "ids=", params$ids,"&format=json", "&time=", time)
      }
    } else if (endpoint_lower == "airport") {
      complete_url <- paste0(base_url, "airport?", "ids=", params$ids, "&format=json")
    }
    
    
    # Fetch and parse JSON data
    response <- httr::GET(complete_url)
    parsed_data <- jsonlite::fromJSON(rawToChar(response$content))
    
    
    # Convert parsed data to tibble
    parsed_tibble <- tibble::as_tibble(parsed_data)
    
    
    # Customize tibble based on the endpoint
    if (endpoint_lower == "metar") {
      parsed_tibble <- dplyr::select(parsed_tibble, metar_id, icaoId, obsTime, reportTime, temp, wspd, visib, name, lat, lon) |>
        dplyr::mutate(visib = stringr::str_remove(visib, "\\+") %>% as.numeric(),state = stringr::str_extract(name, "(?<=, )\\w{2}(?=, US$)")) |>
        dplyr::arrange(-metar_id)
    } else if (endpoint_lower == "airport") {
      parsed_tibble <- parsed_tibble |>
        tidyr::unnest(runways, names_sep = "_") |>
        dplyr::select(icaoId, state, runway_id = runways_id, runway_dimension = runways_dimension, runway_surface = runways_surface, rwyNum, rwyLength, rwyType) |>
        tidyr::separate(runway_dimension, into = c("runway_length", "runway_width"), sep = "x", convert = TRUE)
    }
    
    
    # Return the parsed tibble
    return(parsed_tibble)
  }
  # END URL Building Function #
  
  
  
  
  ################################  
  ## Server Function Definition ##
  ################################
  
  # WIND TAB #
  
  # Reactive expression to fetch data from the API when updateButton is clicked
  metar_data_reactive <- eventReactive(input$updateButton, {
    build_url(
      endpoint = "metar",
      icaoIDs = if (!is.null(input$icaoID) && input$icaoID != "") input$icaoID else "#US",
      hours = if (!is.null(input$hours) && input$hours != "") as.numeric(input$hours) else 24)
  })
  
  # Function to generate dynamic frequency table titles based on icaoID input
  generate_table_title <- function(icaoID) {
  }
  
  
  # Wind tab plot
  output$windPlot <- renderPlot({
    metar_data <- metar_data_reactive()
    
    
    # Ensure 'state' is a factor and 'wspd' is numeric
    metar_data$state <- as.factor(metar_data$state)
    metar_data$wspd <- as.numeric(metar_data$wspd)
    
    
    # Filter out rows with NA in the 'state' column
    metar_data <- metar_data %>% filter(!is.na(state))
    
    
    # Calculate mean wind speed for each state and order descending
    mean_wspd <- metar_data %>%
      group_by(state) %>%
      summarize(mean_wspd = mean(wspd, na.rm = TRUE)) %>%
      arrange(mean_wspd)
    
    
    # Reorder 'state' factor levels by mean wind speed in descending order
    metar_data$state <- factor(metar_data$state, levels = mean_wspd$state)
    
    # Generate the density ridges plot
    ggplot(metar_data, aes(x = wspd, y = state, fill = after_stat(x), group = state)) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, xlim = c(0, 50)) +
      scale_fill_viridis_c(name = "Wind Speed", option = "C") +
      labs(title = 'Wind Speed by State') +
      scale_fill_viridis(direction = -1) + theme_minimal()
  })  
  
  
  # Render the table title
  output$windTableTitle <- renderUI({
    req(input$icaoID)
    h4(generate_table_title(input$icaoID))
  })
  
  # Reactive expression for updating the table when updateButton is clicked
  windTableData <- eventReactive(input$updateButton, {
    metar_data <- metar_data_reactive()
    # Create wind speed ranges
    metar_data <- metar_data %>%
      mutate(wspd_ranges = cut(wspd, breaks = c(0, 5, 10, 15, Inf), labels = c("0-5", "6-10", "11-15", ">15")))
    # Create contingency table
    state_vs_wspd <- table(metar_data$state, metar_data$wspd_ranges)
    # Convert the contingency table to a data frame
    state_vs_wspd_df <- as.data.frame.matrix(state_vs_wspd)
    # Add a state column
    state_vs_wspd_df$State <- rownames(state_vs_wspd_df)
    rownames(state_vs_wspd_df) <- NULL
    # Reorder columns to place state at the front
    state_vs_wspd_df <- state_vs_wspd_df[, c(ncol(state_vs_wspd_df), 1:(ncol(state_vs_wspd_df) - 1))]
    state_vs_wspd_df
  })
  
  # Render the table when updateButton is clicked
  output$windTable <- renderTable({
    windTableData()
  })
  
  # Event handler for the update button
  observeEvent(input$updateButton, {
  })
  
  
  
  # TEMP TAB #
  
  # Reactive expression to fetch data from the API when updateButton is clicked
  metar_data_reactive <- eventReactive(input$updateButton, {
    build_url(
      endpoint = "metar",
      icaoIDs = if (!is.null(input$icaoID) && input$icaoID != "") input$icaoID else "#US",
      hours = if (!is.null(input$hours) && input$hours != "") as.numeric(input$hours) else 24)
  })
  
  
  # Reactive expression to process temperature data from the fetched metar data
  temp_data_reactive <- eventReactive(input$updateButton, {
    metar_data <- metar_data_reactive()
    
    
    # Create a tibble to use with plotting a barchart.  
    temp_data <- metar_data %>%
      filter(!is.na(temp)) %>%
      group_by(state) %>%
      summarise(max_temp = max(temp, na.rm = TRUE),
                min_temp = min(temp, na.rm = TRUE),
                temp_range = max_temp - min_temp) %>%
      pivot_longer(cols = c(max_temp, min_temp), 
                   names_to = "temperature_type", 
                   values_to = "temperature")
    temp_data
  })
  
  
  # Render the temperature bar chart
  output$tempBarChart <- renderPlot({
    temp_data <- temp_data_reactive()
    
    
    # Plot: Bar chart for max and min temperatures
    if (!is.null(temp_data) && nrow(temp_data) > 0) {
      ggplot(temp_data, aes(x = state, y = temperature, fill = temperature_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "State", y = "Temperature (°C)", title = "Max and Min Temperatures by State") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() + theme_minimal() + labs(title = "No data available")
    }
  })
  
  
  # Render the summary table
  output$tempSummaryTable <- renderTable({
    temp_data <- temp_data_reactive()
    
    if (!is.null(temp_data) && nrow(temp_data) > 0) {
      summary_table <- temp_data %>%
        group_by(state) %>%
        summarise(temp_min = min(temperature[temperature_type == "min_temp"], na.rm = TRUE),
                  temp_max = max(temperature[temperature_type == "max_temp"], na.rm = TRUE),
                  temp_range = temp_max - temp_min)
      summary_table
    } else {
      NULL
    }
  })
  
  
  # UI part for summary table
  output$tempSummaryTitle <- renderUI({
    if (!is.null(temp_data_reactive()) && nrow(temp_data_reactive()) > 0) {
      h4("Temperature Summary by State")
    }
  })
  
  
  
  # VISIBILITY TAB #
  
  
  # Reactive expression to fetch data from the API when updateButton is clicked
  metar_data_reactive_visib <- eventReactive(input$updateButton_visib, {
    build_url(
      endpoint = "metar",
      icaoIDs = if (!is.null(input$icaoID_visib) && input$icaoID_visib != "") input$icaoID_visib else "#US",
      hours = if (!is.null(input$hours_visib) && input$hours_visib != "") as.numeric(input$hours_visib) else 24)
  })
  
  
  # Reactive expression to process visibility data from the fetched metar data
  visib_data_reactive <- eventReactive(input$updateButton_visib, {
    metar_data <- metar_data_reactive_visib()
    
    
    # Filter out rows with missing visib values
    visib_data <- metar_data %>%
      filter(!is.na(visib)) %>%
      group_by(state) %>%
      summarise(visib_min = min(visib, na.rm = TRUE),
                visib_max = max(visib, na.rm = TRUE),
                visib_range = visib_max - visib_min)
    
    visib_data
  })
  
  
  # Render the visibility heatmap
  output$visibHeatmap <- renderPlot({
    visib_data <- visib_data_reactive()
    
    
    # Example plot: Heatmap for average visibility by state
    if (!is.null(visib_data) && nrow(visib_data) > 0) {
      ggplot(visib_data, aes(x = state, y = 1, fill = visib_min)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red", name = "Minimum Visibility") +
        labs(x = "State", y = "", title = "Minimum Visibility by State") +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    } else {
      ggplot() + theme_minimal() + labs(title = "No data available")
    }
  })
  
  
  # Render the summary table
  output$visibSummaryTable <- renderTable({
    visib_data <- visib_data_reactive()
    if (!is.null(visib_data) && nrow(visib_data) > 0) {
      visib_data %>%
        select(state, visib_min, visib_max, visib_range)
    } else {
      NULL
    }
  })
  
  # UI part for summary table
  output$visibSummaryTitle <- renderUI({
    if (!is.null(visib_data_reactive()) && nrow(visib_data_reactive()) > 0) {
      h4("Visibility Summary by State")
    }
  })
  
  
  
  # AIRPORT STATISTICS #
  
  # Reactive expression to fetch data from the API when updateButton is clicked
  airport_data_reactive <- eventReactive(input$updateButton, {
    build_url(
      endpoint = "airport",
      icaoIDs = if (!is.null(input$icaoID) && input$icaoID != "") input$icaoID else "#US"
    )
  })
  
  # Render the plot using ggplot in a reactive environment
  output$airport_plot <- renderPlot({
    data <- airport_data_reactive()
    # Ensure runway_length and runway_width are numeric
    data$runway_length <- as.numeric(data$runway_length)
    data$runway_width <- as.numeric(data$runway_width)
    # Convert rwyType to factor if needed
    data$rwyType <- factor(data$rwyType, levels = c("S", "M", "L"))
    
    # Plotting using ggplot with a vertical violin plot
    ggplot(data, aes(y = runway_length, x = runway_width, fill = rwyType)) +
      geom_violin(scale = "width", draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(aes(color = rwyType), width = 0.2, height = 0.2, size = 1.5) +
      scale_fill_manual(values = c("S" = "#66c2a5", "M" = "#fc8d62", "L" = "#8da0cb")) +
      scale_color_manual(values = c("S" = "#66c2a5", "M" = "#fc8d62", "L" = "#8da0cb")) +
      guides(color = guide_legend(title = "Runway Type")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(10, 10, 10, 10, "pt")) +
      labs(
        x = "Runway Length",
        y = "Runway Width",
        fill = "Runway Type",
        title = "Airport Runway Statistics",
        subtitle = "Distribution of runway lengths and widths by type") +
      coord_flip()
  })
  
  
# DATA DOWNLOAD #
  
  # Reactive expression for data table display
    dataToDisplay <- reactive({
      endpoint <- input$endpoint_dd
      icaoIDs <- "#US"
      hours <- 96
      time <- "issue"
      
      # Special consideration and workaround for `taf` endpoint
      if (endpoint == "taf") {
        tafNorth <- build_url(endpoint = "taf", icaoIDs = "@USN", time = "issue")
        tafEast <- build_url(endpoint = "taf", icaoIDs = "@USE", time = "issue")
        tafSouth <- build_url(endpoint = "taf", icaoIDs = "@USS", time = "issue")
        tafWest <- build_url(endpoint = "taf", icaoIDs = "@USW", time = "issue")
        tafALL <- bind_rows(tafNorth, tafEast, tafSouth, tafWest)
        
        # Remove the 'fcsts' column
        data <- tafALL |>
          select(-fcsts)
        
        return(data)
      } else {
        # For other endpoints (metar, airport), fetch data using build_url
        data <- build_url(endpoint = endpoint,
                          icaoIDs = icaoIDs,
                          hours = hours,
                          time = time)
        
        return(data)
      }
    })
    
    
  # Render the table for display
    output$dataTableDownload <- renderTable({
      head(dataToDisplay(), 25)  # Limit to first 25 rows
    })
    
    
  # Download button logic
    output$downloadData <- downloadHandler(
      filename = function() {
        endpoint <- input$endpoint_dd
        fileFormat <- substr(input$format_dd, 2, 5)  # Extract "csv" or "xls"
        paste0(endpoint, "_data.", fileFormat)
      },
      content = function(file) {
        tempData <- dataToDisplay()
        fileFormat <- substr(input$format_dd, 2, 5)
        
        if (fileFormat == "csv") {
          write.csv(tempData, file = file, row.names = FALSE)
        } else if (fileFormat == "xls") {
          writexl::write_xlsx(tempData, file)
        }
      }
    )  

  
    
    # DATA EXPLORATION #
    
    # Creates the dynamic box on tab to choose variables.
    output$var_select_ui <- renderUI({
      if (input$data_source == "metar") {
        selectInput("variables", "Select Variables:",
                    choices = c("temp", "wspd", "visib", "state"),
                    selected = c("temp", "wspd"), multiple = TRUE)
      } else {
        selectInput("variables", "Select Variables:",
                    choices = c("state", "runway_length", "runway_width", "runway_surface", "rwyNum"),
                    selected = c("runway_length", "runway_width"), multiple = TRUE)
      }
    })
    
    # Creates the faceting tool            
    output$facet_var_ui <- renderUI({
      if (input$data_source == "metar") {
        selectInput("facet_var", "Select Facet Variable:",
                    choices = c("None", "state"))
      } else {
        selectInput("facet_var", "Select Facet Variable:",
                    choices = c("None", "state", "runway_surface"))
      }
    })
    
    # Calls the build_url function and sets appropriate parameters
    get_data <- reactive({
      req(input$data_source)
      if (input$data_source == "metar") {
        data <- build_url(endpoint = "metar", hours = 24, icaoIDs = "#US")
      } else {
        data <- build_url(endpoint = "airport", icaoIDs = "#US")
      }
      return(data)
    })
    
    # Creates the plot dynamically depending on the sidebarPanel inputs.       
    output$data_plot <- renderPlot({
      req(input$variables)
      data <- get_data()
      
      # Subset data based on selected variables
      data <- data[, input$variables, drop = FALSE]
      
      # Generate plot
      p <- ggplot(data, aes_string(x = input$variables[1], y = if(length(input$variables) > 1) input$variables[2] else NULL)) +
        {switch(input$plot_type,
                "Scatter Plot" = geom_point(),
                "Bar Plot" = geom_bar(stat = "identity"),
                "Histogram" = geom_histogram(binwidth = 1),  # Use binwidth to prevent potential issues
                "Box Plot" = geom_boxplot())}
      
      # Add faceting if selected
      if (input$facet_var != "None") {
        p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
      }
      
      # Print plot object after faceting for debugging
      print("Plot object after faceting:")
      print(p)
      
      # Render the plot
      print(p)
    })
    
    # Handles the printing of the tibble summary stats beneath the plot.       
    output$data_summary <- renderPrint({
      req(input$variables)
      data <- get_data()
      
      # Check for missing values and data types
      data <- data %>%
        mutate(across(all_of(input$variables), as.numeric, .names = "num_{.col}"))
      
      # Filter out rows with NA values in the numeric columns
      data <- data %>%
        filter(across(starts_with("num_"), ~ !is.na(.)))
      
      # Generate summary based on selected variables and summary type
      summary_data <- data %>%
        summarise(across(starts_with("num_"), 
                         list(mean = ~mean(.x, na.rm = TRUE), 
                              median = ~median(.x, na.rm = TRUE), 
                              sd = ~sd(.x, na.rm = TRUE), 
                              count = ~sum(!is.na(.x))), 
                         .names = "{.fn}_{.col}"))
      
      # Print summary data for debugging
      print("Summary Data:")
      print(summary_data)
      
      # Select and print only the relevant summary type
      summary_type <- switch(input$summary_type,
                             "Mean" = select(summary_data, ends_with("_mean")),
                             "Median" = select(summary_data, ends_with("_median")),
                             "Standard Deviation" = select(summary_data, ends_with("_sd")),
                             "Count" = select(summary_data, ends_with("_count")))
      
      print(summary_type)
    })
    
    
  
# icoaID TAB #
    
  
  # Category descriptions
    descriptions <- list(
      "@TOP" = "Top 39 airports in the US",
      "@TOPE" = "Top airports in the eastern US",
      "@TOPC" = "Top airports in the central US",
      "@TOPW" = "Top airports in the western US",
      "@USN" = "Major airports in the northern US region",
      "@USS" = "Major airports in the southern US region",
      "@USE" = "Major airports in the eastern US region",
      "@USW" = "Major airports in the western US region",
      "#US" = "All airports in the US",
      "<state>" = "All airports by state"
    )
    
  
  # Render category description
    output$category_description <- renderText({
      descriptions[[input$icao_category]]
    })
    
  
  # Generate state dropdown if <state> is selected
    output$state_dropdown <- renderUI({
      if (input$icao_category == "<state>") {
        selectInput("state", "Select State", choices = state.abb)
      }
    })
  
  
  # Filter and display the table based on the selection
    output$icao_table <- renderTable({
      if (input$icao_category %in% c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW")) {
        metar_loc <- build_url(endpoint = "metar",
                               icaoIDs = input$icao_category,
                               hours = 96)
        
        metar_loc <- metar_loc %>%
          select(icaoId, name, state) %>%
          group_by(state) %>%
          arrange(state,name) %>%
          mutate(name = str_remove(name, ",.*")) %>%
          distinct(name, .keep_all = TRUE)
        
      } else if (input$icao_category == "<state>") {
        req(input$state)
        metar_loc <- build_url(endpoint = "metar",
                               icaoIDs = paste0("@", input$state),
                               hours = 96)
        
        metar_loc <- metar_loc %>%
          select(icaoId, name, state) %>%
          group_by(state) %>%
          arrange(name) %>%  # Changed to arrange by name
          mutate(name = str_remove(name, ",.*")) %>%
          distinct(name, .keep_all = TRUE)
      } else {
        return(NULL)  # For #US, do nothing
      }
      metar_loc
    })
}