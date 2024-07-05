### PACKAGE_INSTALLATION ###
#install.packages(c("httr", "jsonlite", "tidyverse", "dplyr"))


### LIBRARY_LOAD ###
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)

### BUILD URL FUNCTION (NO API KEY NEEDED) ###

  # Helper vector for acceptable state abbreviation inputs
  valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  
  # Helper set of vectors for containing valid icaoIDs
  valid_icaoID_1 <- c("KAAA", "KAAF", "KAAO", "KAAS", "KAAT", "KABE", "KABI", "KABQ", "KABR", "KABY", "KACB", "KACJ", "KACK", "KACP", "KACQ", "KACT", "KACV", "KACY", "KACZ", "KADC", "KADF", "KADG", "KADH", "KADM", "KADS", "KADT", "KADU", "KADW", "KAEG", "KAEJ", "KAEL", "KAEX", "KAFF", "KAFJ", "KAFK", "KAFN", "KAFO", "KAFP", "KAFW", "KAGC", "KAGO", "KAGR", "KAGS", "KAGZ", "KAHC", "KAHH", "KAHN", "KAHQ", "KAIA", "KAIB", "KAID", "KAIG", "KAIK", "KAIO", "KAIT", "KAIV", "KAIY", "KAIZ", "KAJG", "KAJO", "KAJR", "KAJZ", "KAKH", "KAKO", "KAKQ", "KAKR", "KALB", "KALI", "KALK", "KALM", "KALN", "KALO", "KALS", "KALW", "KALX", "KAMA", "KAMG", "KAMN", "KAMT", "KAMW", "KANB", "KAND", "KANE", "KANJ", "KANK", "KANP", "KANQ", "KANW", "KANY", "KAOC", "KAOH", "KAOO", "KAOV", "KAPA", "KAPC", "KAPF", "KAPG", "KAPH", "KAPN", "KAPT", "KAPV", "KAQO", "KAQP", "KAQR", "KAQW", "KARA", "KARB", "KARG", "KARM", "KARR", "KART", "KARV", "KARW", "KASD", "KASE", "KASG", "KASH", "KASJ", "KASL", "KASN", "KAST", "KASW", "KASX", "KASY", "KATA", "KATL", "KATS", "KATW", "KATY", "KAUG", "KAUH", "KAUM", "KAUN", "KAUO", "KAUS", "KAUW", "KAVC", "KAVK", "KAVL", "KAVO", "KAVP", "KAVQ", "KAVX", "KAWG", "KAWM", "KAWO", "KAXA", "KAXH", "KAXN", "KAXQ", "KAXS", "KAXV", "KAXX", "KAYS", "KAYX", "KAZC", "KAZE", "KAZO", "KAZU", "KBAB", "KBAC", "KBAD", "KBAF", "KBAK", "KBAM", "KBAN", "KBAX", "KBAZ", "KBBB", "KBBD", "KBBG", "KBBP", "KBBW", "KBCB", "KBCE", "KBCK", "KBCT", "KBDE", "KBDG", "KBDH", "KBDJ", "KBDL", "KBDN", "KBDQ", "KBDR", "KBDU", "KBEC", "KBED", "KBEH", "KBFA", "KBFD", "KBFE", "KBFF", "KBFI", "KBFK", "KBFL", "KBFM", "KBFR", "KBFW", "KBGD", "KBGE", "KBGF", "KBGM", "KBGR", "KBHB", "KBHC", "KBHK", "KBHM", "KBID", "KBIE", "KBIF", "KBIH", "KBIL", "KBIS", "KBIV", "KBIX", "KBJC", "KBJI", "KBJJ", "KBJN", "KBKD", "KBKE", "KBKF", "KBKL", "KBKN", "KBKS", "KBKT", "KBKV", "KBKW", "KBKX", "KBLF", "KBLH", "KBLI", "KBLM", "KBLU", "KBLV", "KBMC", "KBMG", "KBMI", "KBML", "KBMQ", "KBMT", "KBNA", "KBNG", "KBNL", "KBNO", "KBNW", "KBOI", "KBOK", "KBOS", "KBOW", "KBPG", "KBPI", "KBPK", "KBPP", "KBPT", "KBQK", "KBQR", "KBRD", "KBRL", "KBRO", "KBRY", "KBST", "KBTA", "KBTF", "KBTL", "KBTM", "KBTN", "KBTP", "KBTR", "KBTV", "KBTY", "KBUB", "KBUF", "KBUM", "KBUR", "KBUU", "KBUY", "KBVI", "KBVN", "KBVO", "KBVS", "KBVU", "KBVX", "KBVY", "KBWC", "KBWD", "KBWG", "KBWI", "KBWP", "KBXA", "KBXG", "KBXK", "KBXM", "KBYG", "KBYH", "KBYI", "KBYS", "KBYY", "KBZN", "KCAD", "KCAE", "KCAG", "KCAK", "KCAO", "KCAR", "KCAV", "KCBE", "KCBF", "KCBG", "KCBK", "KCBM", "KCCA", "KCCB", "KCCO", "KCCR", "KCCY", "KCDA", "KCDC", "KCDD", "KCDH", "KCDI", "KCDK", "KCDN", "KCDR", "KCDS", "KCDW", "KCEA", "KCEC", "KCEF", "KCEK", "KCEU", "KCEV", "KCEW", "KCEY", "KCEZ", "KCFD", "KCFE", "KCFJ", "KCFS", "KCFT", "KCFV", "KCGC", "KCGE", "KCGF", "KCGI", "KCGS", "KCGX", "KCGZ", "KCHA", "KCHD", "KCHK", "KCHN", "KCHO", "KCHQ", "KCHS", "KCHT", "KCHU", "KCIC", "KCID", "KCII", "KCIN", "KCIR", "KCIU", "KCJJ", "KCJR", "KCKA", "KCKB", "KCKC", "KCKF", "KCKI", "KCKM", "KCKN", "KCKP", "KCKV", "KCLE", "KCLI", "KCLK", "KCLL", "KCLM", "KCLR", "KCLS", "KCLT", "KCLW", "KCMA", "KCMH", "KCMI", "KCMR", "KCMX", "KCMY", "KCNC", "KCNH", "KCNI", "KCNK", "KCNM", "KCNO", "KCNP", "KCNU", "KCNW", "KCNY", "KCOD", "KCOE", "KCOF", "KCOI", "KCOM", "KCON", "KCOQ", "KCOS", "KCOT", "KCOU", "KCPC", "KCPF", "KCPK", "KCPM", "KCPR", "KCPS", "KCPT", "KCPU", "KCQA", "KCQB", "KCQC", "KCQF", "KCQM", "KCQW", "KCQX", "KCRE", "KCRG", "KCRO", "KCRP", "KCRQ", "KCRS", "KCRT", "KCRW", "KCRX", "KCRZ", "KCSB", "KCSG", "KCSM", "KCSQ", "KCSV", "KCTB", "KCTJ", "KCTK", "KCTY", "KCTZ", "KCUB", "KCUH", "KCUL", "KCUT", "KCVG", "KCVH", "KCVK", "KCVN", "KCVO", "KCVS", "KCVX", "KCWA", "KCWC", "KCWF", "KCWI", "KCWS", "KCWV", "KCXE", "KCXL", "KCXO", "KCXP", "KCXU", "KCXW", "KCXY", "KCYO", "KCYS", "KCYW", "KCZD", "KCZG", "KCZK", "KCZL", "KCZT", "KDAA", "KDAB", "KDAF", "KDAG", "KDAL", "KDAN", "KDAW", "KDAY", "KDBN", "KDBQ", "KDCA", "KDCU", "KDCY", "KDDC", "KDDH", "KDEC", "KDED", "KDEH", "KDEN", "KDEQ", "KDET", "KDEW", "KDFI", "KDFW", "KDGL", "KDGW", "KDHN");
  valid_icaoID_2 <- c("KDHT", "KDIJ", "KDIK", "KDKB", "KDKK", "KDKR", "KDKX", "KDLC", "KDLF", "KDLH", "KDLL", "KDLN", "KDLO", "KDLS", "KDLZ", "KDMA", "KDMN", "KDMO", "KDMW", "KDNA", "KDNL", "KDNN", "KDNS", "KDNV", "KDOV", "KDPA", "KDPG", "KDPL", "KDQH", "KDRA", "KDRI", "KDRO", "KDRT", "KDRU", "KDSM", "KDSV", "KDTA", "KDTG", "KDTL", "KDTN", "KDTO", "KDTS", "KDTW", "KDUA", "KDUC", "KDUG", "KDUH", "KDUJ", "KDUX", "KDVK", "KDVL", "KDVN", "KDVO", "KDVP", "KDVT", "KDWH", "KDWU", "KDXE", "KDXR", "KDXX", "KDYA", "KDYB", "KDYL", "KDYR", "KDYS", "KDYT", "KDZJ", "KEAG", "KEAN", "KEAR", "KEAT", "KEAU", "KEBG", "KEBS", "KECG", "KECP", "KECS", "KECU", "KEDC", "KEDE", "KEDG", "KEDJ", "KEDN", "KEDU", "KEDW", "KEED", "KEEN", "KEEO", "KEET", "KEFC", "KEFD", "KEFK", "KEFT", "KEFW", "KEGE", "KEGI", "KEGQ", "KEGT", "KEGV", "KEHA", "KEHO", "KEHR", "KEIK", "KEIW", "KEKA", "KEKM", "KEKN", "KEKO", "KEKQ", "KEKS", "KEKX", "KEKY", "KELA", "KELD", "KELK", "KELM", "KELN", "KELO", "KELP", "KELY", "KELZ", "KEMM", "KEMP", "KEMT", "KEMV", "KEND", "KENL", "KENV", "KENW", "KEOE", "KEOK", "KEOP", "KEOS", "KEPG", "KEPH", "KEPM", "KEQA", "KEQY", "KERI", "KERR", "KERV", "KERY", "KESC", "KESF", "KESN", "KEST", "KESW", "KETB", "KETC", "KETN", "KEUF", "KEUG", "KEUL", "KEVB", "KEVM", "KEVU", "KEVV", "KEVW", "KEVY", "KEWB", "KEWK", "KEWN", "KEWR", "KEXX", "KEYE", "KEYF", "KEYQ", "KEYW", "KEZF", "KEZI", "KEZM", "KEZS", "KEZZ", "KFAF", "KFAM", "KFAR", "KFAT", "KFAY", "KFBG", "KFBL", "KFBR", "KFBY", "KFCA", "KFCH", "KFCI", "KFCM", "KFCS", "KFCT", "KFCY", "KFDK", "KFDR", "KFDW", "KFDY", "KFEP", "KFES", "KFET", "KFFA", "KFFC", "KFFL", "KFFM", "KFFO", "KFFT", "KFFZ", "KFGU", "KFGX", "KFHB", "KFHR", "KFHU", "KFIG", "KFIT", "KFKA", "KFKL", "KFKN", "KFKR", "KFKS", "KFLD", "KFLG", "KFLL", "KFLO", "KFLP", "KFLV", "KFLX", "KFLY", "KFME", "KFMH", "KFMM", "KFMN", "KFMY", "KFMZ", "KFNB", "KFNL", "KFNT", "KFOA", "KFOD", "KFOE", "KFOK", "KFOM", "KFOT", "KFOZ", "KFPK", "KFPR", "KFQD", "KFRG", "KFRH", "KFRI", "KFRM", "KFRR", "KFSD", "KFSE", "KFSI", "KFSK", "KFSM", "KFSO", "KFST", "KFSU", "KFSW", "KFTG", "KFTK", "KFTT", "KFTW", "KFTY", "KFUL", "KFVE", "KFVX", "KFWA", "KFWC", "KFWN", "KFWQ", "KFWS", "KFXE", "KFXY", "KFYE", "KFYJ", "KFYM", "KFYV", "KFZG", "KFZI", "KFZY", "KGAB", "KGAD", "KGAF", "KGAG", "KGAI", "KGAO", "KGBD", "KGBR", "KGCC", "KGCD", "KGCK", "KGCM", "KGCN", "KGDJ", "KGDM", "KGDP", "KGDV", "KGDY", "KGED", "KGEG", "KGEU", "KGEV", "KGEY", "KGFA", "KGFK", "KGFL", "KGGE", "KGGF", "KGGG", "KGGI", "KGGW", "KGHG", "KGHM", "KGIC", "KGIF", "KGJT", "KGKJ", "KGKT", "KGKY", "KGLD", "KGLH", "KGLS", "KGLW", "KGMJ", "KGMU", "KGNB", "KGNC", "KGNF", "KGNG", "KGNI", "KGNT", "KGNV", "KGOK", "KGON", "KGOO", "KGOV", "KGPI", "KGPT", "KGPZ", "KGRB", "KGRD", "KGRF", "KGRI", "KGRK", "KGRN", "KGRR", "KGSB", "KGSO", "KGSP", "KGSW", "KGTB", "KGTE", "KGTF", "KGTG", "KGTR", "KGTU", "KGUC", "KGUP", "KGUS", "KGUY", "KGVE", "KGVQ", "KGVT", "KGWB", "KGWO", "KGWR", "KGWS", "KGWW", "KGXA", "KGXF", "KGXY", "KGYB", "KGYH", "KGYI", "KGYR", "KGYY", "KGZH", "KGZL", "KHAB", "KHAE", "KHAF", "KHAI", "KHAO", "KHAR", "KHBC", "KHBG", "KHBI", "KHBR", "KHCR", "KHBZ", "KHCD", "KHDE", "KHDN", "KHDO", "KHEE", "KHEF", "KHEG", "KHEI", "KHEQ", "KHEY", "KHEZ", "KHFD", "KHFF", "KHFJ", "KHGR", "KHHF", "KHHR", "KHHW", "KHIB", "KHIE", "KHIF", "KHII", "KHIO", "KHJH", "KHJO", "KHKA", "KHKS", "KHKY", "KHLC", "KHLG", "KHLN", "KHLR", "KHLX", "KHMN", "KHMS", "KHMT", "KHMZ", "KHND", "KHNZ", "KHOB", "KHOE", "KHON", "KHOP", "KHOT", "KHOU", "KHPN", "KHQG", "KHQM", "KHQU", "KHQZ", "KHRI", "KHRJ", "KHRL", "KHRO", "KHRT", "KHRU", "KHRX", "KHSA", "KHSE", "KHSI", "KHSP", "KHSR", "KHST", "KHSV", "KHTH", "KHTO", "KHTS", "KHUA", "KHUF", "KHUL", "KHUM", "KHUT", "KHVC", "KHVE", "KHVN", "KHVR", "KHVS", "KHWD", "KHWO", "KHWQ", "KHWV", "KHWY", "KHXD", "KHXF", "KHYA", "KHYI", "KHYR", "KHYS", "KHYW", "KHYX", "KHZE", "KHZL", "KIAB", "KIAD", "KIAG", "KIAH", "KIBM", "KICR", "KICT", "KIDA", "KIDI", "KIDL", "KIDP", "KIEN", "KIFP", "KIGM", "KIGX", "KIIB", "KIIY", "KIJD", "KIJX", "KIKV", "KIKW", "KILE", "KILG", "KILM");
  valid_icaoID_3 <- c("KILN", "KIML", "KIMM", "KIMS", "KIMT", "KIND", "KINJ", "KINK", "KINL", "KINS", "KINT", "KINW", "KIOW", "KIPJ", "KIPL", "KIPT", "KIRK", "KISM", "KISN", "KISO", "KISP", "KISW", "KITH", "KITR", "KIWA", "KIWI", "KIWD", "KIXD", "KIYK", "KIZA", "KIZG", "KJAC", "KJAN", "KJAX", "KJBR", "KJCT", "KJDN", "KJEF", "KJFK", "KJFX", "KJER", "KJGG", "KJHN", "KJHW", "KJKA", "KJLN", "KJMS", "KJNX", "KJQF", "KJRA", "KJRB", "KJST", "KJSV", "KJVW", "KJWG", "KJWN", "KJYO", "KJYR", "KJZI", "KJZP", "KKIC", "KKLS", "KKNB", "KKY8", "KLAA", "KLAF", "KLAL", "KLAM", "KLAN", "KLAR", "KLAS", "KLAW", "KLAX", "KLBB", "KLBE", "KLBF", "KLBL", "KLBR", "KLBT", "KLBX", "KLCG", "KLCH", "KLCI", "KLCK", "KLCQ", "KLDJ", "KLDM", "KLEB", "KLEE", "KLEM", "KLEW", "KLEX", "KLFI", "KLFK", "KLFT", "KLGA", "KLGB", "KLGD", "KLGF", "KLGU", "KLHB", "KLHM", "KLHQ", "KLHV", "KLHW", "KLHX", "KLHZ", "KLIC", "KLIT", "KLIZ", "KLKP", "KLKR", "KLKU", "KLKV", "KLLJ", "KLLQ", "KLLU", "KLMO", "KLMS", "KLMT", "KLNA", "KLNC", "KLND", "KLNK", "KLNN", "KLNP", "KLNS", "KLOL", "KLOR", "KLOT", "KLOU", "KLOZ", "KLPC", "KLPR", "KLQK", "KLQR", "KLRD", "KLRF", "KLRG", "KLRU", "KLSB", "KLSE", "KLSF", "KLSK", "KLSN", "KLSV", "KLTS", "KLTY", "KLUF", "KLUG", "KLUK", "KLUL", "KLVK", "KLVL", "KLVM", "KLVN", "KLVS", "KLWB", "KLWC", "KLWL", "KLWM", "KLWS", "KLWT", "KLWV", "KLXL", "KLXN", "KLXT", "KLXV", "KLYH", "KLYO", "KLZU", "KLZZ", "KMAC", "KMAE", "KMAF", "KMAI", "KMAL", "KMAN", "KMAO", "KMAW", "KMBG", "KMBO", "KMBS", "KMBT", "KMCB", "KMCC", "KMCE", "KMCF", "KMCI", "KMCK", "KMCN", "KMCO", "KMCW", "KMCZ", "KMDD", "KMDQ", "KMDS", "KMDT", "KMDW", "KMDZ", "KMEB", "KMEI", "KMEJ", "KMEM", "KMER", "KMEV", "KMFE", "KMFI", "KMFR", "KMFV", "KMGE", "KMGG", "KMGJ", "KMGM", "KMGW", "KMGY", "KMHE", "KMHK", "KMHL", "KMHN", "KMHR", "KMHS", "KMHT", "KMHV", "KMIA", "KMIB", "KMIC", "KMIO", "KMIT", "KMIV", "KMJX", "KMKA", "KMKC", "KMKE", "KMKG", "KMKJ", "KMKL", "KMKO", "KMKT", "KMKY", "KMLB", "KMLC", "KMLD", "KMLE", "KMLF", "KMLI", "KMLS", "KMLT", "KMLU", "KMMH", "KMMI", "KMMK", "KMML", "KMMS", "KMMT", "KMMU", "KMMV", "KMNI", "KMNZ", "KMOB", "KMOD", "KMOR", "KMOT", "KMPE", "KMPI", "KMPJ", "KMPO", "KMPR", "KMPV", "KMQI", "KMQJ", "KMQS", "KMQY", "KMRB", "KMRF", "KMRH", "KMRN", "KMRY", "KMSL", "KMSN", "KMSO", "KMSP", "KMSS", "KMSV", "KMSY", "KMTC", "KMTH", "KMTJ", "KMTN", "KMTP", "KMTV", "KMTW", "KMUO", "KMUT", "KMUU", "KMVC", "KMVI", "KMVL", "KMVM", "KMVY", "KMWH", "KMWC", "KMWK", "KMWL", "KMXA", "KMXF", "KMXO", "KMYF", "KMYL", "KMYR", "KMYV", "KMYZ", "KMZJ", "KNAB", "KNBC", "KNBG", "KNBJ", "KNBW", "KNCA", "KNDY", "KNDZ", "KNEL", "KNEN", "KNEW", "KNFD", "KNFE", "KNFG", "KNFJ", "KNFL", "KNFW", "KNGP", "KNGS", "KNGU", "KNGZ", "KNHK", "KNHL", "KNHZ", "KNID", "KNIP", "KNJK", "KNJM", "KNJW", "KNKL", "KNKT", "KNKX", "KNLC", "KNMM", "KNOW", "KNPA", "KNPI", "KNQA", "KNQB", "KNQX", "KNRA", "KNRB", "KNRN", "KNRQ", "KNRS", "KNSE", "KNSI", "KNTD", "KNTK", "KNTU", "KNUC", "KNUI", "KNUN", "KNUQ", "KNUW", "KNVI", "KNWL", "KNYG", "KNYL", "KNXF", "KNXP", "KNXX", "KNZJ", "KNZY", "KOAJ", "KOAK", "KOAR", "KOBE", "KOBI", "KOCF", "KOCH", "KOCW", "KODO", "KODX", "KOEL", "KOFF", "KOFK", "KOFP", "KOGA", "KOGB", "KOGD", "KOGS", "KOIC", "KOIN", "KOJA", "KOJC", "KOKB", "KOKC", "KOKK", "KOKM", "KOKS", "KOKV", "KOLD", "KOLE", "KOLF", "KOLM", "KOLS", "KOLU", "KOLV", "KOLZ", "KOMA", "KOMH", "KOMK", "KOMN", "KONA", "KONL", "KONM", "KONO", "KONP", "KONT", "KONX", "KOPF", "KOPL", "KOQN", "KOQU", "KORB", "KORD", "KORE", "KORF", "KORG", "KORH", "KORL", "KORS", "KOSC", "KOSH", "KOSU", "KOSX", "KOTH", "KOTM", "KOUN", "KOVE", "KOVS", "KOWB", "KOWD", "KOWI", "KOWK", "KOXB", "KOXC", "KOXD", "KOXR", "KOYM", "KOZA", "KOZR", "KOZS", "KOZW", "KPAE", "KPAH", "KPAM", "KPAN", "KPAO", "KPBF", "KPBG", "KPBI", "KPBX", "KPCM", "KPCU", "KPCW", "KPCZ", "KPDC", "KPDK", "KPDT", "KPDX", "KPEO", "KPEQ", "KPFC", "KPFN", "KPGA", "KPGD", "KPGR", "KPGV", "KPHD", "KPHF", "KPHG", "KPHH", "KPHK", "KPHL", "KPHP", "KPHT", "KPHX", "KPIA", "KPIB", "KPIE", "KPIH", "KPIR", "KPIT", "KPKB", "KPKV", "KPLB", "KPLK", "KPLN", "KPLR");
  valid_icaoID_4 <- c("KPLU", "KPMB", "KPMD", "KPMV", "KPMZ", "KPNA", "KPNC", "KPNE", "KPNM", "KPNN", "KPNS", "KPOB", "KPOC", "KPOU", "KPOY", "KPPA", "KPPF", "KPQI", "KPQL", "KPRB", "KPRC", "KPRN", "KPRX", "KPSB", "KPSC", "KPSF", "KPSK", "KPSM", "KPSN", "KPSO", "KPSP", "KPTB", "KPTD", "KPTK", "KPTN", "KPTS", "KPTT", "KPTV", "KPTW", "KPUB", "KPUC", "KPUJ", "KPUW", "KPVB", "KPVC", "KPVD", "KPVE", "KPVF", "KPVG", "KPVJ", "KPVU", "KPVW", "KPWA", "KPWD", "KPWK", "KPWM", "KPWT", "KPXE", "KPYG", "KPYM", "KPYP", "KPYX", "KQA7", "KQAD", "KQAE", "KQAJ", "KQAO", "KQAQ", "KQAX", "KQAY", "KQCO", "KQCT", "KQCU", "KQD9", "KQDM", "KQEZ", "KQGV", "KQGX", "KQIR", "KQIU", "KQL5", "KQMA", "KQMG", "KQMH", "KQNC", "KQNN", "KQNS", "KQNT", "KQNY", "KQOS", "KQPC", "KQPD", "KQRY", "KQSA", "KQSE", "KQSL", "KQSM", "KQSR", "KQTA", "KQTI", "KQTO", "KQTU", "KQTX", "KQTZ", "KQVO", "KQWM", "KQXJ", "KQXN", "KQYB", "KRAC", "KRAL", "KRAP", "KRAW", "KRBD", "KRBE", "KRBG", "KRBL", "KRBM", "KRBW", "KRCA", "KRCE", "KRCM", "KRCT", "KRDD", "KRDG", "KRDM", "KRDR", "KRDU", "KRED", "KREG", "KREI", "KREO", "KRFD", "KRGK", "KRHI", "KRHP", "KRHV", "KRIC", "KRID", "KRIF", "KRIL", "KRIR", "KRIU", "KRIV", "KRIW", "KRJD", "KRKR", "KRKD", "KRKS", "KRLD", "KRME", "KRMN", "KRND", "KRNM", "KRNO", "KRNT", "KRNV", "KROA", "KROC", "KROG", "KROW", "KRPB", "KRPD", "KRPH", "KRPX", "KRQE", "KRRT", "KRSL", "KRST", "KRSW", "KRTN", "KRTS", "KRUE", "KRUG", "KRUQ", "KRUT", "KRVL", "KRVS", "KRWI", "KRWL", "KRWV", "KRXE", "KRYN", "KRYV", "KRYW", "KRYY", "KRZL", "KRZN", "KRZT", "KRZZ", "KSAA", "KSAC", "KSAD", "KSAF", "KSAN", "KSAS", "KSAT", "KSAV", "KSAW", "KSAZ", "KSBA", "KSBD", "KSBM", "KSBN", "KSBO", "KSBP", "KSBS", "KSBX", "KSBY", "KSCB", "KSCD", "KSCH", "KSCK", "KSCR", "KSDC", "KSDF", "KSDL", "KSDM", "KSDY", "KSEA", "KSEE", "KSEF", "KSEG", "KSEM", "KSEP", "KSEZ", "KSFB", "KSFD", "KSFF", "KSFM", "KSFO", "KSFQ", "KSFZ", "KSGF", "KSGJ", "KSGT", "KSGU", "KSHD", "KSHN", "KSHR", "KSHV", "KSIF", "KSIK", "KSIY", "KSJC", "KSJN", "KSJT", "KSKA", "KSKF", "KSKI", "KSKX", "KSLB", "KSLC", "KSLE", "KSLG", "KSLI", "KSLK", "KSLN", "KSLR", "KSMD", "KSME", "KSMF", "KSMN", "KSMO", "KSMQ", "KSMS", "KSMX", "KSNA", "KSNC", "KSNK", "KSNL", "KSNS", "KSNT", "KSNY", "KSOA", "KSOP", "KSOW", "KSPA", "KSPB", "KSPD", "KSPF", "KSPG", "KSPH", "KSPI", "KSPS", "KSPW", "KSPX", "KSPZ", "KSQL", "KSRC", "KSRQ", "KSRR", "KSSC", "KSSF", "KSSI", "KSSN", "KSSQ", "KSTC", "KSTF", "KSTK", "KSTL", "KSTP", "KSTS", "KSUA", "KSUN", "KSUS", "KSUT", "KSUU", "KSUW", "KSUX", "KSUZ", "KSVC", "KSVE", "KSVH", "KSWF", "KSWI", "KSWO", "KSWT", "KSWW", "KSXL", "KSXT", "KSXU", "KSYF", "KSYI", "KSYL", "KSYN", "KSYR", "KSZL", "KSZP", "KSZT", "KTAD", "KTAN", "KTBN", "KTBR", "KTBX", "KTCC", "KTCL", "KTCM", "KTCS", "KTCY", "KTDF", "KTDO", "KTDW", "KTEB", "KTEL", "KTEX", "KTGI", "KTHM", "KTHP", "KTHV", "KTIK", "KTIW", "KTIX", "KTKI", "KTKO", "KTKV", "KTLH", "KTLR", "KTMB", "KTME", "KTMK", "KTNP", "KTNT", "KTNU", "KTNX", "KTOA", "KTOI", "KTOL", "KTOP", "KTOR", "KTPA", "KTPF", "KTPH", "KTPL", "KTQE", "KTQH", "KTQK", "KTRI", "KTRK", "KTRM", "KTRX", "KTSP", "KTTA", "KTTD", "KTTF", "KTTN", "KTUL", "KTUP", "KTUS", "KTVC", "KTVF", "KTVL", "KTVR", "KTVY", "KTVZ", "KTWF", "KTWT", "KTXK", "KTXW", "KTYL", "KTYQ", "KTYR", "KTYS", "KTZR", "KTZT", "KUAO", "KUBE", "KUBS", "KUCA", "KUCP", "KUDD", "KUDG", "KUES", "KUGN", "KUIL", "KUIN", "KUKF", "KUKI", "KUKL", "KUKT", "KULS", "KUMP", "KUNI", "KUNU", "KUNV", "KUOS", "KUOX", "KUTA", "KUTS", "KUUU", "KUVA", "KUZA", "KVAY", "KVBG", "KVBT", "KVBW", "KVCB", "KVCT", "KVCV", "KVDF", "KVEL", "KVER", "KVES", "KVGT", "KVIH", "KVIS", "KVJI", "KVKS", "KVKX", "KVLD", "KVLL", "KVMR", "KVNC", "KVNY", "KVPS", "KVPZ", "KVQQ", "KVRB", "KVSF", "KVTA", "KVTN", "KVUJ", "KVUO", "KVVS", "KVYS", "KWAL", "KWAY", "KWBW", "KWDG", "KWDR", "KWHP", "KWJF", "KWLD", "KWLW", "KWMC", "KWRB", "KWRI", "KWRL", "KWST", "KWVI", "KWVL", "KWWD", "KWWR", "KWYS", "KXBP", "KXFL", "KXLL", "KXMR", "KXNA", "KXNO", "KXNX", "KXTA", "KXVG", "KXWA", "KYIP", "KYKM", "KYKN", "KYNG", "KZEF", "KZER", "KZPH", "KZUN", "KZZV");
  
  # Helper function to check ICAO IDs format
  validate_icaoIDs <- function(icaoIDs) {
    valid_state <- icaoIDs %in% valid_states
    valid_icao_single <- grepl("^[A-Z]{4}$", icaoIDs)
    valid_icao_multiple <- grepl("^([A-Z]{4}[ ,]?)+$", icaoIDs)
    return(valid_state || valid_icao_single || valid_icao_multiple)}

  # Helper function to check ICAO ID is valid
  check_icao_in_valid_vectors <- function(icaoID) {
    all_valid_icaoIDs <- c(valid_icaoID_1, valid_icaoID_2, valid_icaoID_3, valid_icaoID_4)
    return(icaoID %in% all_valid_icaoIDs)
  }
  
  # URL building function
  # If calling endpoint `metar`, you can optionally call `icaoIDs` and hours`
  # If calling endpoint `taf`, you must specify an `icaoIDs` and optionally a `time`
  # If calling endpoint `airport`, you must specify an `icaoIDs`.
  build_url <- function(base_url = "https://aviationweather.gov/api/data/", 
                      endpoint, 
                      icaoIDs = NULL, 
                      hours = NULL, 
                      time = NULL) {
    # Define the valid endpoints
    valid_endpoints <- c("metar", "taf", "airport")
    
    # Convert endpoint to lowercase for internal processing
    endpoint_lower <- tolower(endpoint)
    
    # Check if the provided endpoint is valid
    if (!(endpoint_lower %in% valid_endpoints)) {
      stop("Invalid endpoint. Choose from: ", paste(valid_endpoints, collapse = ", "))
    }
    
    # Validate ICAO IDs format if provided and required
    if (!is.null(icaoIDs)) {
      if (!validate_icaoIDs(icaoIDs)) {
        stop("Invalid icaoIDs. Must be a two-letter state abbreviation, a four-letter all uppercase ICAO ID, or multiple four-letter all uppercase ICAO IDs separated by commas or spaces.")
      }
      
      # Validate individual ICAO IDs against predefined vectors
      if (!icaoIDs %in% valid_states) {
        if (grepl("^([A-Z]{4}[ ,]?)+$", icaoIDs, ignore.case = TRUE)) {
          icaoID_list <- unlist(strsplit(icaoIDs, "[ ,]+"))
          invalid_ids <- icaoID_list[!sapply(icaoID_list, check_icao_in_valid_vectors)]
          if (length(invalid_ids) > 0) {
            stop("The following ICAO IDs are not valid: ", paste(invalid_ids, collapse = ", "))
          }
        } else if (!check_icao_in_valid_vectors(icaoIDs)) {
          stop("The ICAO ID is not valid: ", icaoIDs)
        }
      }
    } else if (endpoint_lower %in% c("taf", "airport")) {
      stop("icaoIDs parameter is required for endpoints 'TAF' and 'airport'.")
    }
    
    # Validate hours for METAR
    if (endpoint_lower == "metar" && !is.null(hours)) {
      if (!(is.numeric(hours) && hours == as.integer(hours) && hours > 0)) {
        stop("Invalid hours. Must be a positive whole number.")
      }
    }
    
    # Validate time for TAF
    if (endpoint_lower == "taf" && !is.null(time)) {
      if (!(time %in% c("issue", "valid"))) {
        stop("Invalid time. Must be 'issue' or 'valid'.")
      }
    }
    
    # Initialize the query parameters
    params <- list(format = "json")
    
    # Add ids parameter if provided
    if (!is.null(icaoIDs)) {
      if (endpoint_lower %in% c("metar", "taf", "airport")) {
        if (icaoIDs %in% valid_states) {
          params$ids <- paste0("@", icaoIDs)
        } else if (grepl("^[A-Z]{4}$", icaoIDs, ignore.case = TRUE)) {
          params$ids <- icaoIDs
        } else if (grepl("^([A-Z]{4}[ ,]?)+$", icaoIDs, ignore.case = TRUE)) {
          icaoIDs_cleaned <- str_replace_all(icaoIDs, "[ ,]+", " ")
          params$ids <- icaoIDs_cleaned
        }
      }
    }
    
    # Add specific parameters based on the endpoint
    if (endpoint_lower == "metar" && !is.null(hours)) {
      params$hours <- hours
    } else if (endpoint_lower == "taf" && !is.null(time)) {
      params$time <- time
    }
    
    # Construct the complete URL with query parameters
    complete_url <- modify_url(paste0(base_url, endpoint_lower), query = params)
    
    return(complete_url)
  }

  
icaoST = "WY"
#args(build_url)
metar_json_URL <- build_url(endpoint = "metar", icaoIDs = icaoST, hours = 2)
metar_dataGET <- httr::GET(metar_json_URL)
metar_parsed <- fromJSON(rawToChar(metar_dataGET$content))
metar_parsed_tibb <- as_tibble(metar_parsed)
metar_parsed_tibb <- metar_parsed_tibb |> filter(str_starts(icaoId,"K"))
metar_parsed_tibb

taf_json_URL <- build_url(endpoint = "taf", icaoIDs = icaoST, hours = 2, time = "issue")
taf_dataGET <- httr::GET(taf_json_URL)
taf_parsed <- fromJSON(rawToChar(taf_dataGET$content))
taf_parsed_tibb <- as_tibble(taf_parsed)
taf_parsed_tibb

airport_json_URL <- build_url(endpoint = "airport", icaoIDs = icaoST)
airport_dataGET <- httr::GET(airport_json_URL)
airport_parsed <- fromJSON(rawToChar(airport_dataGET$content))
airport_parsed_tibb <- as_tibble(airport_parsed)
airport_parsed_tibb


METAR_dataset <- metar_parsed_tibb |>
  select(metar_id, icaoId, obsTime, temp, dewp, wdir, wspd, visib) |>
  filter(str_starts(icaoId, "K"))

joined_data <- metar_parsed_tibb |> left_join(airport_parsed_tibb, by = "icaoId")

#TABS
#Wind Speed METAR
#Visibility METAR
#Temperature METAR + TAF
#Airport Statistics AIRPORT

#CONTINGENCY TABLES


#NUMERICAL SUMMARIES


#PLOTS
#gganimate plot that compares 

#visib
#wspd

