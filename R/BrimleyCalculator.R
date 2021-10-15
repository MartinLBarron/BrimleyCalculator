#' Calculate Brimley/Cacoon line statistics
#'
#' @param birtdate Date of person
#'
#' @return date person will cross Brimley/Cacoon line
#' @export
#' @import glue scales
#' @examples
#' results <- BrimleyCalculator(as.Date("1973-12-18"))
BrimleyCalculator <- function(birthdate){

  #18,530 is the BrimleyCacoon line
  brimleyLine <- 18530

  currentDate <- Sys.Date()

  #How many days old is person
  daysOld <- currentDate-birthdate
  daysOldf <- format(as.numeric(daysOld), big.mark=",")

  #When will they cross line
  dateReachLine=birthdate+brimleyLine

  #Days until line
  daysUntil = brimleyLine - daysOld
  daysUntilf = format(as.numeric(daysUntil), big.mark=",")


  #% of Brimley
  percentBrimley <- paste0(round(daysOld/brimleyLine*100, digits=1),"%")

  art <- c(
             "                                                                                ",
             "                                    .:lc::;,'.                                  ",
             "                                    .lO000000ko,     ....                       ",
             "                                     .':cok00O0Oc.   ...,;.                     ",
             "                   .''.             ..';cloodkO0Okdcc;,;cdd'                    ",
             "                .;oxxo:;,.           .'lxkOxodk00000OkddO00Oo;.                 ",
             "               'dOOxl:,.                .'clloxk0000000000000Oxl:'              ",
             "              ,x0x:.                           .,oO0KKKKKKK000000Od;.           ",
             "           .,ckkc.                                'd00KK0K00KKK00000d'          ",
             "          .;dkc.                                   .l0000000KK0000000d.         ",
             "         .';d:                                      .cO00000000xdO000O:         ",
             "         .;dc                                        .x0000000x'.o0000k'        ",
             "         ;xl.                                        .d0O00000c  :O0000c        ",
             "        ,xc.                                         .d0000000:  ;O0000l.       ",
             "        :l.                                    ..''';oO0000000c  .lO000x'       ",
             "        :,                                      .,:ldO000000Okc    .:oxk:       ",
             "        ..                                    ..';:cok000000k:.       .,,       ",
             "                                                 .;;;lO0000d;.                  ",
             "                                               .,d0K000000O;          ,,        ",
             "                   .                           ,c;:odx0K00k,          ..        ",
             "                 .:ddoc;;;,.                     ..',lO000Oko:.       ..  ...   ",
             "                .,ooldO000Okolcc;,,,,,,,,,,;,:codxxkO000000Ol'.    .,lxko::l,   ",
             "                '.   .lO000OO000xl'.......'cdk00O0000000Odoxc...  .,lOOo,..:.   ",
             "      ',    .,;:,  'cloO000OOOOO00Oc',coold00000Oxk000koxo',xOx:    .o:   .o;   ",
             "      ;o.    :k; .'co;.;dxdclk0Odlkc  .okkOKK000kclxkl''ll' ;x:      ..   'x:   ",
             "      ,o.     '.   .:c,',:okOxxd..:.  .d000KKK000kc:;.  .   ,l'      ;c.  ,d'   ",
             "      .c' .         'okOOOxc,;c'      .xK00KKK00Ol'..       lk:.     cOxc.',    ",
             "       ..':.          ......::.       .dK00KKK00kl;;.      ,cc.      :kk0l.     ",
             "         .,.           .',,;...'.     .oK000K00OO00Oc.   .;.  .      ;lcd,      ",
             "                       .....';,.       c000000Kx;',,'.   .           :lc,       ",
             "                           ..'.        cO00000KO;                   .cl'        ",
             "                            ,o'       .d00000000kl.                 .l;  'c:;'. ",
             "       ....                ;xl..'.  .;x00000000000d.                .d:  l000Oko",
             "  .';ldxOO;               'xo.    .:xkxkkO00000000o.                ,k; 'x00OO00",
             "cok0K00000l.              .:,.    'c;. ..':oold00OOd,               cOdlx0000000",
             "0000K00000Oc               'l,                .;c;:kk:.            ;k00000000000",
             "000000O0000k,              '.      .',.      .,,.  .lkx;          ,k000000000000",
             "000000O00000x,                   .;x0Okl,..',ccc;  . ,kk'        ;k0000000000000",
             "000000000KK00k,          ', .. 'oooO000OOo:dodkdx:.'. ,dl,     .lO0O000000000000",
             "0000000KK0K00O:..      .;o,.oxok00OO000OO00000000Odl' .oOOc...,x0000000000000000",
             "0KK0KK000000KO:ld'     .,clxkxoclc;;oxxxkO0000OOOOOx;,lk00kc'cO00000000000000000",
             "0KK0K0000000KOco0k:    .,;;'.         ....;:;lkOOOOxxO000k;.lO000000000000000000",
             "0KKKK0O00000KOco00k;                         .'d000O00000xlx00OO0000000000O00000",
             "0KK000O0000000:;k0Ox,                       ;l;o00000000000000OOO000000000OO0000",
             "0KK000000000K0c.l0O0k:            ..      .lk000OOOO0000OOO000OO0000000000000000",
             "0KK0KK00000000o.'x0O0Ol.        .,dx;..;:lk0OO0OOOOO0000OOO000OO0000000000000000",
             "00KKKK00000000x. :O0OOOd,        .;xOkkOOO00OO00O00000000O0000000000000000000000",
             "00K000000KK000O; .d00000k:         c0OOOOOO0OO0000000000000000000000000000000000"
  )
  #Print results
  cat(art, sep="\n")
  cat("\n")
  cat(paste0("a person born on ", format(birthdate, "%B %d, %Y"), ":\n"))
  cat(paste0("\t- is ", daysOldf, " days old\n"))
  cat(paste0("\t- is ",percentBrimley, " the age Wilford Brimley was when Cacoon premeired\n"))
  cat(paste0("\t- will reach the Brimley/Cacoon Line on ", format(dateReachLine, "%B %d, %Y"), "\n"))
  cat(paste0("\t- will reach line in ", daysUntilf, " days\n"))

  return(dt)
}

# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1973-12-18"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1971-01-20"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("1973-08-26"))
# results <- BrimleyCalculator::BrimleyCalculator(as.Date("2006-08-26"))


