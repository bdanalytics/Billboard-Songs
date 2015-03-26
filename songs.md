# Billboard Top10: Top10 classification
bdanalytics  

**  **    
**Date: (Thu) Mar 26, 2015**    

# Introduction:  

Data: Wikipedia, Billboard.com, EchoNest
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/songs.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "condition"          # "condition" or "sample"
glb_split_newdata_condition <- "year >= 2010"    # or NULL
glb_split_newdata_size <- 0.25               # > 0 & < 1
glb_split_sample.seed <- 1000               # or any integer 

glb_predct_var <- "Top10"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("songID")                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature) ; num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                        c("songtitle", "artistname", "artistID")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("year")     # or NULL
                                      )

glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- glb_dmy_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/songs.csv", 
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
```

```
## [1] "Reading file ./data/songs.csv..."
## [1] "dimensions of data in ./data/songs.csv: 7,574 rows x 39 cols"
```

```r
print(table(glb_entity_df$year))
```

```
## 
## 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 
##  328  196  186  324  198  258  178  329  380  357  363  282  518  434  479 
## 2005 2006 2007 2008 2009 2010 
##  392  479  622  415  483  373
```

```r
print(table(glb_entity_df$artistname))
```

```
## 
##                                           50 Cent 
##                                                 7 
##                                        98 Degrees 
##                                                 3 
##                                 A Day to Remember 
##                                                 9 
##                                  A Static Lullaby 
##                                                 8 
##                                           Aaliyah 
##                                                15 
##                                     Aaron Neville 
##                                                 1 
##                                             AC/DC 
##                                                12 
##                                       Ace Frehley 
##                                                10 
##                                       Ace of Base 
##                                                10 
##                                       Ace Of Base 
##                                                 2 
##                                Ace Troubleshooter 
##                                                11 
##                                      Adam Lambert 
##                                                 1 
##                                      Adina Howard 
##                                                 1 
##                                         Aerosmith 
##                                                16 
##                                           After 7 
##                                                 1 
##                                       Against Me! 
##                                                 9 
##                                    Agnostic Front 
##                                                12 
##                                        Aimee Mann 
##                                                11 
##                                               Air 
##                                                 6 
##                                              Akon 
##                                                14 
##                                       Al B. Sure! 
##                                                 1 
##                                 Alanis Morissette 
##                                                12 
##                                     Alannah Myles 
##                                                 1 
##                                        Alex Lloyd 
##                                                13 
##                                             Alias 
##                                                 1 
##                                       Alicia Keys 
##                                                 6 
##                                     Alkaline Trio 
##                                                11 
##                                        All Saints 
##                                                 1 
##                                         All-4-One 
##                                                 3 
##                                            Allure 
##                                                 1 
##                                          Aly & AJ 
##                                                12 
##                                    American Hi-Fi 
##                                                11 
##                           American Idol Finalists 
##                                                 1 
##                                            Amerie 
##                                                 1 
##                                         Amy Grant 
##                                                 3 
##                                     Amy Winehouse 
##                                                 1 
##                                         Anal Cunt 
##                                                49 
##                                  Analog Rebellion 
##                                                13 
##                                         Anastacia 
##                                                11 
##                                    Andrea Bocelli 
##                                                 7 
##                                 Angels & Airwaves 
##                                                10 
##                                    Angie Martinez 
##                                                11 
##                                      Ani DiFranco 
##                                                12 
##                                    Another Animal 
##                                                12 
##                              Another Bad Creation 
##                                                 2 
##                                           Anthrax 
##                                                14 
##                                              Aqua 
##                                                 1 
##                                          Aqueduct 
##                                                12 
##                                        Arch Enemy 
##                                                11 
##                              Arrested Development 
##                                                 3 
##                                 Artists For Haiti 
##                                                 1 
##                                               Ash 
##                                                18 
##                                           Ashanti 
##                                                 4 
##                                    Ashlee Simpson 
##                                                 1 
##                                Atari Teenage Riot 
##                                                10 
##                                           Athlete 
##                                                10 
##                                    Atlantic Starr 
##                                                 1 
##                                        Audioslave 
##                                                14 
##                                  August Burns Red 
##                                                11 
##                                           Autolux 
##                                                11 
##                                 Avenged Sevenfold 
##                                                22 
##                                   Avoid One Thing 
##                                                13 
##                                     Avril Lavigne 
##                                                16 
##                                            Az Yet 
##                                                 1 
##                     B_la Fleck and the Flecktones 
##                                                11 
##                                             B.o.B 
##                                                 3 
##                                  Babes in Toyland 
##                                                 6 
##                                         Baby Bash 
##                                                 2 
##                                          Babyface 
##                                                 3 
##                                   Backstreet Boys 
##                                                 5 
##                                       Bad English 
##                                                 1 
##                                          Baha Men 
##                                                11 
##                                    Band of Skulls 
##                                                11 
##                                              Bane 
##                                                 7 
##                                  Barenaked Ladies 
##                                                 1 
##                                     Barry Manilow 
##                                                10 
##                                              Beck 
##                                                 1 
##                                        Beenie Man 
##                                                13 
##                                    Bell Biv Devoe 
##                                                 2 
##                                         Ben Folds 
##                                                 5 
##                                        Ben Harper 
##                                                 9 
##                                       Ben Kweller 
##                                                11 
##                                      Bette Midler 
##                                                 1 
##                                   Bettie Serveert 
##                                                12 
##                                           Beyonce 
##                                                10 
##                                 Beyonce & Shakira 
##                                                 1 
##                                         Big Sugar 
##                                                12 
##                                       Billy Bragg 
##                                                11 
##                                        Billy Joel 
##                                                 2 
##                                   Billy Ray Cyrus 
##                                                 1 
##                                               Bis 
##                                                12 
##                                        Biz Markie 
##                                                 1 
##                                        Bizzy Bone 
##                                                 8 
##                                         Black Box 
##                                                 2 
##                                   Black Eyed Peas 
##                                                 1 
##                                        Black Kids 
##                                                10 
##                               Black Label Society 
##                                                13 
##                                     Black Sabbath 
##                                                13 
##                                       BLACKstreet 
##                                                 1 
##                                     BLACKstreet ( 
##                                                 1 
##                                       Blaqk Audio 
##                                                10 
##                                            Blaque 
##                                                 2 
##                            Blessid Union Of Souls 
##                                                 1 
##                                         Blink-182 
##                                                 1 
##                                         Blockhead 
##                                                11 
##                                    Blonde Redhead 
##                                                 8 
##                                      Blu Cantrell 
##                                                 1 
##                                    Blues Traveler 
##                                                14 
##                                              Blur 
##                                                 3 
##                                           Bo Bice 
##                                                 1 
##                                  Boards of Canada 
##                                                 9 
##                                         Bob Dylan 
##                                                20 
##                                       Bobby Brown 
##                                                 2 
##                                   Bobby Valentino 
##                                                 1 
##                                          Bon Jovi 
##                                                 2 
##                              Bone Thugs-n-Harmony 
##                                                23 
##                              Bone Thugs-N-Harmony 
##                                                 2 
##                                      Bonnie Raitt 
##                                                 1 
##                                Boo-Yaa T.R.I.B.E. 
##                                                 1 
##                                            Boston 
##                                                10 
##                                           Bow Wow 
##                                                 3 
##                                       Boyz II Men 
##                                                14 
##                                        Boz Scaggs 
##                                                 8 
##                                      Brad Mehldau 
##                                                 5 
##                                            Brandy 
##                                                 5 
##                                           Brandy  
##                                                 1 
##                                   Brandy & Monica 
##                                                 1 
##                                    Brian McKnight 
##                                                22 
##                                       Bright Eyes 
##                                                 4 
##                                    Britney Spears 
##                                                 8 
##                                     Brooks & Dunn 
##                                                22 
##                                        Brownstone 
##                                                 1 
##                                   Bruce Dickinson 
##                                                 9 
##                                     Bruce Hornsby 
##                                                 7 
##                                 Bruce Springsteen 
##                                                11 
##                                       Bryan Adams 
##                                                 2 
##                                     Bubba Sparxxx 
##                                                 1 
##                                        Buckcherry 
##                                                 1 
##                                        Buckethead 
##                                                 1 
##                                         Buddy Guy 
##                                                 8 
##                                    Built to Spill 
##                                                10 
##                                      Busta Rhymes 
##                                                 2 
##                       Busta Rhymes & Mariah Carey 
##                                                 1 
##                                      Butch Walker 
##                                                11 
##                                  Butthole Surfers 
##                                                13 
##                                 C+C Music Factory 
##                                                 2 
##                                              Cake 
##                                                13 
##                                          Calexico 
##                                                 4 
##                                          Calloway 
##                                                 1 
##                                           Cam'ron 
##                                                 2 
##                                    Candy Butchers 
##                                                12 
##                                          Candyman 
##                                                 1 
##                                   Cannibal Corpse 
##                                                 6 
##                                       Carbon Leaf 
##                                                10 
##                                           Caribou 
##                                                11 
##                                       Carly Simon 
##                                                13 
##                                   Carmen Rasmusen 
##                                                 5 
##                                  Carrie Underwood 
##                                                 4 
##                                           Cascada 
##                                                 1 
##                                              Case 
##                                                 1 
##                                             Case  
##                                                 1 
##                                           Cassidy 
##                                                 1 
##                                            Cassie 
##                                                 1 
##                                         Cat Power 
##                                                12 
##                                          Catch 22 
##                                                12 
##                                         Cathedral 
##                                                 4 
##                                   Catherine Wheel 
##                                                11 
##                                      Cathy Dennis 
##                                                 3 
##                                     CeCe Peniston 
##                                                 1 
##                                       Celine Dion 
##                                                 6 
##                                          Cellador 
##                                                 8 
##                                               Cex 
##                                                11 
##                                      Chad Kroeger 
##                                                 1 
##                                    Chamillionaire 
##                                                 1 
##                                    Changing Faces 
##                                                 2 
##                               Charlotte Hatherley 
##                                                12 
##                                              Cher 
##                                                 2 
##                                       Cheryl Cole 
##                                                10 
##                                    Chesney Hawkes 
##                                                 1 
##                                          Chevelle 
##                                                11 
##                                           Chicago 
##                                                 1 
##                                            Chingy 
##                                                 4 
##                                       Chris Brown 
##                                                 6 
##                                       Chris Isaak 
##                                                12 
##                                         Chris Rea 
##                                                13 
##                                 Chrisette Michele 
##                                                10 
##                                Christina Aguilera 
##                                                 8 
##                                  Christina Milian 
##                                                 1 
##                                       Chumbawamba 
##                                                21 
##                                             Ciara 
##                                                 4 
##                                         City High 
##                                                 1 
##                                        Clay Aiken 
##                                                 2 
##                                    Cobra Starship 
##                                                 1 
##                                    Colbie Caillat 
##                                                 1 
##                                          Coldplay 
##                                                 9 
##                                   Collective Soul 
##                                                10 
##                                     Color Me Badd 
##                                                 2 
##                                    Comets on Fire 
##                                                 7 
##                                            Coolio 
##                                                 1 
##                                            Corina 
##                                                 1 
##                                        Cornershop 
##                                                24 
##                                       Craig David 
##                                                 1 
##                                        Craig Mack 
##                                                 1 
##                                      Craig Morgan 
##                                                10 
##                                Crash Test Dummies 
##                                                 1 
##                                        Crazy Town 
##                                                14 
##                                             Creed 
##                                                16 
##                                         Crime Mob 
##                                                12 
##                                         Crooked X 
##                                                10 
##                                      Cult of Luna 
##                                                10 
##                                    Curtis Stigers 
##                                                 1 
##                                            D.H.T. 
##                                                 1 
##                                          D'Angelo 
##                                                 1 
##                                               D12 
##                                                 1 
##                                               D4L 
##                                                 1 
##                                           Da Brat 
##                                                 1 
##                                         Daft Punk 
##                                                 8 
##                                      Damn Yankees 
##                                                 1 
##                                        Dan Deacon 
##                                                 9 
##                                Daniel Bedingfield 
##                                                 1 
##                                     Daniel Powter 
##                                                 1 
##                                       Danity Kane 
##                                                 2 
##                                 Dark Tranquillity 
##                                                 6 
##                            Dashboard Confessional 
##                                                11 
##                                          Daughtry 
##                                                 2 
##                                      Dave Burrell 
##                                                 6 
##                                       Dave Davies 
##                                                10 
##                                Dave Matthews Band 
##                                                23 
##                                   David Archuleta 
##                                                 1 
##                                      David Banner 
##                                                 1 
##                                       David Byrne 
##                                                23 
##                                        David Cook 
##                                                 1 
##                                   David Coverdale 
##                                                11 
##                                      David Guetta 
##                                                 1 
##                                     Daz Dillinger 
##                                                 7 
##                                     Dead Kennedys 
##                                                10 
##                                      Deana Carter 
##                                                 8 
##                                             Death 
##                                                 8 
##                                       Death Angel 
##                                                 8 
##                                    Debelah Morgan 
##                                                 1 
##                                       Deborah Cox 
##                                                 1 
##                                         Deee-Lite 
##                                                11 
##                               Deep Blue Something 
##                                                 1 
##                                       Deep Forest 
##                                                13 
##                                       Deep Purple 
##                                                11 
##                                          Deerhoof 
##                                                10 
##                                          Deftones 
##                                                 8 
##                                        Del Amitri 
##                                                 1 
##                           Demi Lovato & Joe Jonas 
##                                                 1 
##                                      Depeche Mode 
##                                                 1 
##                                           Des'ree 
##                                                10 
##                                   Destiny's Child 
##                                                22 
##                                             Diddy 
##                                                 2 
##                                              Dido 
##                                                 1 
##                                      Dimmu Borgir 
##                                                10 
##                                              Dino 
##                                                 1 
##                                     Dionne Farris 
##                                                 1 
##                                           Disfear 
##                                                10 
##                                         Dishwalla 
##                                                12 
##                                            Divine 
##                                                 1 
##                                          Divinyls 
##                                                 1 
##                                      Dixie Chicks 
##                                                 6 
##                                  DJ Sammy & Yanou 
##                                                 1 
##                                               DMC 
##                                                 4 
##                              Dogs Die in Hot Cars 
##                                                10 
##                                            Dokken 
##                                                11 
##                                            Domino 
##                                                 1 
##                             Donavon Frankenreiter 
##                                                11 
##                                      Donell Jones 
##                                                 1 
##                                       Donna Lewis 
##                                                 1 
##                                           Dr. Dog 
##                                                10 
##                                         Dr. Dooom 
##                                                19 
##                                           Dr. Dre 
##                                                 2 
##                                       DragonForce 
##                                                 4 
##                                             Drake 
##                                                 3 
##                                             Dream 
##                                                 1 
##                                     Dream Theater 
##                                                 5 
##                                     Drowning Pool 
##                                                11 
##                                               DRS 
##                                                 1 
##                                          Dru Hill 
##                                                 3 
##                                             Duels 
##                                                11 
##                                       Duran Duran 
##                                                 2 
##                                       Dying Fetus 
##                                                 8 
##                                  Eagle-Eye Cherry 
##                                                 1 
##                                            Eagles 
##                                                17 
##                                      Eden's Crush 
##                                                 1 
##                                             Edguy 
##                                                10 
##                                      Edwin McCain 
##                                                 1 
##                                              Eels 
##                                                37 
##                                         Eiffel 65 
##                                                 1 
##                                            Eisley 
##                                                 4 
##                                          Elastica 
##                                                10 
##                                        Elton John 
##                                                 1 
##                                           Embrace 
##                                                10 
##                                               eMC 
##                                                20 
##                                               EMF 
##                                                 1 
##                                      Emily Osment 
##                                                11 
##                                            Eminem 
##                                                24 
##                                          En Vogue 
##                                                 3 
##                                    Energy Orchard 
##                                                10 
##                                            Enigma 
##                                                 2 
##                                  Enrique Iglesias 
##                                                 4 
##                                          Entombed 
##                                                12 
##                                      Enuff Z'nuff 
##                                                14 
##                                              Enya 
##                                                 1 
##                                             Epica 
##                                                 8 
##                                           Erasure 
##                                                11 
##                                      Eric Clapton 
##                                                18 
##                                       Erykah Badu 
##                                                 2 
##                                           Estelle 
##                                                 1 
##                                            Europe 
##                                                 9 
##                                       Evanescence 
##                                                 3 
##                                               Eve 
##                                                 2 
##                                         Everclear 
##                                                14 
##                                          Evergrey 
##                                                 9 
##                           Everything But The Girl 
##                                                 1 
##                                            Exodus 
##                                                10 
##                                            Expose 
##                                                 1 
##                                           Extreme 
##                                                12 
##                                          Fabolous 
##                                                 4 
##                                       Faith Evans 
##                                                 2 
##                                        Faith Hill 
##                                                29 
##                                     Faith No More 
##                                                 1 
##                                    Family Force 5 
##                                                13 
##                                          Fantasia 
##                                                 1 
##                                 Far*East Movement 
##                                                 1 
##                                           Fat Joe 
##                                                 2 
##                                      Fear Factory 
##                                                10 
##                                            Fergie 
##                                                 5 
##                                         Field Mob 
##                                                 1 
##                                     Finger Eleven 
##                                                 1 
##                                        Fireflight 
##                                                10 
##                                          Firehose 
##                                                26 
##                                         Firehouse 
##                                                 2 
##                                              Five 
##                                                17 
##                           Flight of the Conchords 
##                                                25 
##                                          Flo Rida 
##                                                 5 
##                                    Flogging Molly 
##                                                11 
##                                        Fort Minor 
##                                                10 
##                                        Foxy Brown 
##                                                 1 
##                                         Frankie J 
##                                                 1 
##                                              Fuel 
##                                                11 
##                              Fun Lovin' Criminals 
##                                                14 
##                                           G. Love 
##                                                 9 
##                                         Gabrielle 
##                                                11 
##                                      Gavin DeGraw 
##                                                 1 
##                                           Genesis 
##                                                10 
##                                   George Harrison 
##                                                11 
##                                    George Michael 
##                                                 5 
##                                     George Strait 
##                                                 5 
##                                           Gerardo 
##                                                 1 
##                                          Ginuwine 
##                                                 3 
##                                         Glee Cast 
##                                                 2 
##                                    Gloria Estefan 
##                                                 3 
##                                    Gnarls Barkley 
##                                                 1 
##                                        Goldfinger 
##                                                 5 
##                                     Goo Goo Dolls 
##                                                12 
##                                       Gorilla Zoe 
##                                                14 
##                                            Gossip 
##                                                10 
##                                     Great Big Sea 
##                                                13 
##                                         Green Day 
##                                                 2 
##                                     Groove Theory 
##                                                 1 
##                                     Guns N' Roses 
##                                                24 
##                                            Guster 
##                                                13 
##                                      Gwen Stefani 
##                                                 4 
##                                  Gym Class Heroes 
##                                                 1 
##                                            H-Town 
##                                                 1 
##                                          Haddaway 
##                                                 9 
##                      Handsome Boy Modeling School 
##                                                 6 
##                                    Hannah Montana 
##                                                 1 
##                                            Hanson 
##                                                14 
##                                     Harry Connick 
##                                                10 
##                                     Haste the Day 
##                                                11 
##                                             Heart 
##                                                11 
##                                Heavy D & the Boyz 
##                                                12 
##                                           Hi-Five 
##                                                 1 
##                                       Hilary Duff 
##                                                 8 
##                                            Hinder 
##                                                 1 
##                                    Hit the Lights 
##                                                 4 
##                                              Hole 
##                                                12 
##                                        Hoobastank 
##                                                 1 
##                             Hootie & The Blowfish 
##                                                 3 
##                                    House of Lords 
##                                                 7 
##                                     House Of Pain 
##                                                 1 
##                           Huey Lewis and the News 
##                                                 9 
##                                   Hurricane Chris 
##                                                 1 
##                                         Hypocrisy 
##                                                10 
##                                        Ian Gillan 
##                                                12 
##                                        Iced Earth 
##                                                 9 
##                                          Immature 
##                                                 1 
##                                       Imogen Heap 
##                                                12 
##                                         In Flames 
##                                                12 
##                                           Incubus 
##                                                 1 
##                                        India.Arie 
##                                                13 
##                                Infectious Grooves 
##                                                16 
##                                 Ingrid Michaelson 
##                                                12 
##                                      Inner Circle 
##                                                 1 
##                                              Inoj 
##                                                 1 
##                                              INXS 
##                                                 2 
##                                   Isobel Campbell 
##                                                12 
##                                              Iyaz 
##                                                 1 
##                                            J-Kwon 
##                                                 1 
##                                        J. Holiday 
##                                                14 
##                                           Ja Rule 
##                                                 5 
##                                       Jagged Edge 
##                                                 2 
##                                            Jaheim 
##                                                14 
##                                             James 
##                                                11 
##                                       James Blunt 
##                                                 1 
##                                        Jamie Foxx 
##                                                 2 
##                                             Janet 
##                                                 4 
##                                     Janet Jackson 
##                                                10 
##                                      Jason Aldean 
##                                                11 
##                                      Jason Derulo 
##                                                 3 
##                                      Jason DeRulo 
##                                                 1 
##                                        Jason Mraz 
##                                                14 
##                                          Jay Sean 
##                                                 2 
##                                             Jay-Z 
##                                                15 
##                                      Jay-Z & T.I. 
##                                                 1 
##                               Jay-Z + Alicia Keys 
##                                                 1 
##                                Jay-Z + Mr. Hudson 
##                                                 1 
##                                         Jeff Beck 
##                                                11 
##                                    Jennifer Lopez 
##                                                21 
##                              Jennifer Love Hewitt 
##                                                11 
##                                    Jennifer Paige 
##                                                 1 
##                                           Jeremih 
##                                                 1 
##                                    Jerry Cantrell 
##                                                12 
##                                   Jesse McCartney 
##                                                 1 
##                                      Jesse Powell 
##                                                 1 
##                                      Jessi Colter 
##                                                10 
##                                   Jessica Simpson 
##                                                 1 
##                                       Jesus Jones 
##                                                10 
##                                             Jewel 
##                                                 1 
##                                             Jibbs 
##                                                 1 
##                                         Jim Jones 
##                                                 1 
##                                   Jimmy Eat World 
##                                                 5 
##                                      Joan Osborne 
##                                                10 
##                                            Jodeci 
##                                                 2 
##                                       Jody Watley 
##                                                 1 
##                                               Joe 
##                                                10 
##                                        Joe Public 
##                                                 1 
##                                      Joe Satriani 
##                                                13 
##                                     Joey McIntyre 
##                                                 1 
##                                       John Lennon 
##                                                13 
##                                        John Mayer 
##                                                12 
##                                   John Mellencamp 
##                                                14 
##                                       Johnny Gill 
##                                                 1 
##                                              JoJo 
##                                                 1 
##                                            Jon B. 
##                                                 1 
##                                        Jon Secada 
##                                                 2 
##                                    Jonas Brothers 
##                                                 2 
##                                     Jordan Knight 
##                                                 1 
##                                     Jordin Sparks 
##                                                 2 
##                                   Joseph Williams 
##                                                11 
##                                          JT Money 
##                                                 1 
##                                      Judas Priest 
##                                                10 
##                                     Juelz Santana 
##                                                 1 
##                                          Julien-K 
##                                                13 
##                                     Justin Bieber 
##                                                 8 
##                                 Justin Timberlake 
##                                                 6 
##                                          Juvenile 
##                                                 1 
##                                             K-Ci  
##                                                 1 
##                                       K-Ci & JoJo 
##                                                 1 
##                                      K.P. & Envyi 
##                                                 1 
##                                     Kaiser Chiefs 
##                                                11 
##                                            Kalmah 
##                                                 9 
##                                        Kanye West 
##                                                 6 
##                                Kardinal Offishall 
##                                                 1 
##                                       Karyn White 
##                                                 1 
##                                           Kashmir 
##                                                 9 
##                                         Kate Nash 
##                                                13 
##                                        Katy Perry 
##                                                 5 
##                                             Ke$ha 
##                                                 4 
##                                       Keith Sweat 
##                                                 2 
##                                             Kelis 
##                                                 1 
##                                    Kelly Clarkson 
##                                                 8 
##                                         Ken Block 
##                                                 9 
##                                             Kenna 
##                                                12 
##                                           Kenny G 
##                                                 1 
##                                       Keri Hilson 
##                                                 1 
##                                      Kevin Lyttle 
##                                                 1 
##                                      Kevin Rudolf 
##                                                10 
##                                     Kevon Edmonds 
##                                                 1 
##                                      Keyshia Cole 
##                                                 1 
##                                          Kid Cudi 
##                                                 1 
##                                          Kid Rock 
##                                                 1 
##                                     Kidz Bop Kids 
##                                                16 
##                                     Killah Priest 
##                                                13 
##                                      King Crimson 
##                                                 3 
##                                          King's X 
##                                                12 
##                                     Kings Of Leon 
##                                                 1 
##                                              Kiss 
##                                                 1 
##                                             KMFDM 
##                                                 7 
##                                 Kool and the Gang 
##                                                 4 
##                                        Kris Allen 
##                                                12 
##                                        Kris Kross 
##                                                 1 
##                                     Kristin Hersh 
##                                                15 
##                                       Kurt Elling 
##                                                 4 
##                                            Kurupt 
##                                                 8 
##                                     Kylie Minogue 
##                                                18 
##                                         La Bouche 
##                                                 1 
##                                           La Roux 
##                                                 1 
##                                   Lady Antebellum 
##                                                 1 
##                                         Lady Gaga 
##                                                 6 
##                                         Lady GaGa 
##                                                 3 
##                                          Lagwagon 
##                                                25 
##                                       Lauryn Hill 
##                                                 1 
##                                          Le Tigre 
##                                                13 
##                                       LeAnn Rimes 
##                                                18 
##                                      Led Zeppelin 
##                                                13 
##                                               Len 
##                                                 1 
##                                       Lene Marlin 
##                                                10 
##                                     Lenny Kravitz 
##                                                13 
##                                       Leona Lewis 
##                                                 1 
##                                     Leonard Cohen 
##                                                16 
##                                      Les Savy Fav 
##                                                12 
##                                     Life of Agony 
##                                                12 
##                                         Lifehouse 
##                                                13 
##                                           Lil Jon 
##                                                 1 
##                      Lil Jon & The East Side Boyz 
##                                                 2 
##                                          Lil Mama 
##                                                 2 
##                                       Lil Scrappy 
##                                                14 
##                                         Lil Wayne 
##                                                 4 
##                                         Lil' Flip 
##                                                 1 
##                                          Lil' Kim 
##                                                 1 
##                                        Lil' Romeo 
##                                                 1 
##                                       Limp Bizkit 
##                                                26 
##                                     Lindsay Lohan 
##                                                11 
##                                            Linear 
##                                                 1 
##                                       Linkin Park 
##                                                14 
##                                     Lionel Richie 
##                                                25 
##                                   Lisa Stansfield 
##                                                 1 
##                                         Liz Phair 
##                                                13 
##                                         LL Cool J 
##                                                29 
##                                             Lloyd 
##                                                 1 
##                                       Lloyd Banks 
##                                                 1 
##                                           Lock Up 
##                                                14 
##                                        Londonbeat 
##                                                 1 
##                                          Lonestar 
##                                                 9 
##                           Lord Tariq & Peter Gunz 
##                                                 1 
##                                         Los Lobos 
##                                                 6 
##                                   Los Lonely Boys 
##                                                12 
##                                      Lostprophets 
##                                                12 
##                                          Lou Bega 
##                                                 1 
##                                         Lou Gramm 
##                                                 1 
##                                               LSG 
##                                                 1 
##                                          Ludacris 
##                                                15 
##                                       Luis Miguel 
##                                                 7 
##                                             Luniz 
##                                                 1 
##                                       Lupe Fiasco 
##                                                 1 
##                                   Luther Vandross 
##                                                 2 
##                                  Luther Vandross  
##                                                 1 
##                                     Lyfe Jennings 
##                                                12 
##                                            M.I.A. 
##                                                 1 
##                                         Macy Gray 
##                                                 1 
##                                       Mad Caddies 
##                                                19 
##                                           Madonna 
##                                                18 
##                                          Manafest 
##                                                 9 
##                              Manchester Orchestra 
##                                                 1 
##                            Manic Street Preachers 
##                                                22 
##                                      Marc Anthony 
##                                                 2 
##                                  Marcy Playground 
##                                                 1 
##                                      Mariah Carey 
##                                                28 
##                        Mariah Carey & Boyz II Men 
##                                                 1 
##                                             Mario 
##                                                 2 
##                                      Mario Winans 
##                                                 1 
##                                    Marissa Nadler 
##                                                 8 
##                                      Mark Lanegan 
##                                                12 
##                                     Mark Morrison 
##                                                 1 
##                                         Mark Owen 
##                                                13 
##                                       Marky Mark  
##                                                 2 
##                                          Maroon 5 
##                                                 1 
##                                           Martika 
##                                                 1 
##                                     Mary J. Blige 
##                                                 3 
##                                              Mase 
##                                                 3 
##                                   matchbox twenty 
##                                                 3 
##                                       Maxi Priest 
##                                                 1 
##                                           Maxwell 
##                                                 1 
##                                            Mayhem 
##                                                 8 
##                                             McFly 
##                                                13 
##                                         Meat Loaf 
##                                                 6 
##                                         Melanie C 
##                                                12 
##                                 Melissa Etheridge 
##                                                 1 
##                                           Melvins 
##                                                23 
##                                          Menomena 
##                                                12 
##                                       Mercury Rev 
##                                                10 
##                                   Meredith Brooks 
##                                                 1 
##                                 Merril Bainbridge 
##                                                 1 
##                                           Merzbow 
##                                                 3 
##                                         Metallica 
##                                                 1 
##                                        Method Man 
##                                                 1 
##                                     Metro Station 
##                                                 1 
##                                    Michael Bolton 
##                                                 5 
##                                     Michael Bubl_ 
##                                                13 
##                                   Michael Jackson 
##                                                18 
##                                  Michael W. Smith 
##                                                 1 
##                                   Michelle Branch 
##                                                 1 
##                                  Michelle Shocked 
##                                                11 
##                                        Mike Jones 
##                                                15 
##                                       Mike Posner 
##                                                 1 
##                                       Miley Cyrus 
##                                                14 
##                                       Millencolin 
##                                                10 
##                                     Milli Vanilli 
##                                                 1 
##                                              Mims 
##                                                 1 
##                      Ministry and Co-Conspirators 
##                                                 8 
##                                    Mint Condition 
##                                                 1 
##                                       Miss Kittin 
##                                                 1 
##                                     Missy Elliott 
##                                                 1 
##                                         MoKenStef 
##                                                 1 
##                                            Moloko 
##                                                12 
##                                            Monica 
##                                                 4 
##                                           Monifah 
##                                                 1 
##                                    Monster Magnet 
##                                                 8 
##                                    Montell Jordan 
##                                                 2 
## Montell Jordan Feat. Master P & Silkk The Shocker 
##                                                 1 
##                                          Morphine 
##                                                13 
##                                       Motley Crue 
##                                                 1 
##                                           Mr. Big 
##                                                 1 
##                                              MxPx 
##                                                27 
##                               My Chemical Romance 
##                                                11 
##                                            Mystic 
##                                                16 
##                                          N.O.R.E. 
##                                                 1 
##                                         Nada Surf 
##                                                11 
##                                            Nadine 
##                                                12 
##                                      Napalm Death 
##                                                37 
##                                  Natalie Merchant 
##                                                 1 
##                               Natasha Bedingfield 
##                                                 2 
##                                 Natural Selection 
##                                                 1 
##                                 Naughty By Nature 
##                                                 3 
##                                             Ne-Yo 
##                                                 5 
##                                        Neal Morse 
##                                                 9 
##                                        Neil Young 
##                                                10 
##                                               Nek 
##                                                11 
##                                             Nelly 
##                                                20 
##                                     Nelly Furtado 
##                                                 4 
##                                            Nelson 
##                                                 1 
##                                       New Edition 
##                                                 2 
##                             New Kids On The Block 
##                                                 2 
##                                   Newton Faulkner 
##                                                18 
##                                              Next 
##                                                 2 
##                                       Nia Peeples 
##                                                 1 
##                                       Nick Lachey 
##                                                 1 
##                                        Nickelback 
##                                                 6 
##                                      Nicki French 
##                                                 1 
##                                            Nicole 
##                                                 1 
##                                          Nina Sky 
##                                                 1 
##                                         Nine Days 
##                                                 1 
##                                           Nirvana 
##                                                17 
##                                             Nivea 
##                                                 1 
##                                         No Angels 
##                                                13 
##                                          No Doubt 
##                                                 3 
##                                          No Mercy 
##                                                 1 
##                                 No Use for a Name 
##                                                 9 
##                                Noah and the Whale 
##                                                11 
##                                              NOFX 
##                                                17 
##                                            O-Town 
##                                                 2 
##                                            O.A.R. 
##                                                25 
##                                             Oasis 
##                                                 1 
##                                Ocean Colour Scene 
##                                                23 
##                                      October Tide 
##                                                 7 
##                                       Of Montreal 
##                                                12 
##                                          Old 97's 
##                                                11 
##                                   Old Man's Child 
##                                                 7 
##                                       Oleta Adams 
##                                                12 
##                                              Onyx 
##                                                17 
##                 Orchestral Manoeuvres in the Dark 
##                                                11 
##                                           OutKast 
##                                                 4 
##                                           Outlawz 
##                                                17 
##                                          Overkill 
##                                                 9 
##                                          Owl City 
##                                                 1 
##                                        Oysterband 
##                                                11 
##                                          P. Diddy 
##                                                 1 
##                               P. Diddy & Ginuwine 
##                                                 1 
##                                         P.M. Dawn 
##                                                 2 
##                                            P.O.D. 
##                                                 2 
##                               Panic! At The Disco 
##                                                 1 
##                                          Paperboy 
##                                                 1 
##                                       Pat Benatar 
##                                                12 
##                                    Paul McCartney 
##                                                10 
##                                      Paul Stanley 
##                                                 5 
##                                        Paul Young 
##                                                 1 
##                                       Paula Abdul 
##                                                 3 
##                                        Paula Cole 
##                                                 1 
##                                          Pavement 
##                                                12 
##                                         Pearl Jam 
##                                                 1 
##                                           Pebbles 
##                                                 1 
##                                         Pennywise 
##                                                13 
##                                    Pete Townshend 
##                                                18 
##                                       Petey Pablo 
##                                                 1 
##                                    Phantom Planet 
##                                                11 
##                                          Pharrell 
##                                                 1 
##                                      Phil Collins 
##                                                 4 
##                                             Phish 
##                                                 6 
##                                       Pilot Speed 
##                                                10 
##                                              Pink 
##                                                 8 
##                                           Pitbull 
##                                                 2 
##                                   Plain White T's 
##                                                 1 
##                                Plants and Animals 
##                                                11 
##                                   Plastilina Mosh 
##                                                 8 
##                                             Plies 
##                                                 2 
##                                               Poe 
##                                                16 
##                                            Poison 
##                                                15 
##                                   Poison the Well 
##                                                12 
##                                     Powerman 5000 
##                                                26 
##                                        Prefuse 73 
##                                                25 
##                                      Pretty Ricky 
##                                                 1 
##                                            Prince 
##                                                 4 
##               Prince And The New Power Generation 
##                                                 1 
##                               Public Announcement 
##                                                 1 
##                                    Puddle Of Mudd 
##                                                 1 
##                          Puff Daddy & Faith Evans 
##                                                 1 
##                                          Puscifer 
##                                                 7 
##                                    Quad City DJ's 
##                                                 1 
##                                             Queen 
##                                                 1 
##                                     Queen Latifah 
##                                                10 
##                                       Queensryche 
##                                                 1 
##                                          R. Kelly 
##                                                 8 
##                                            R.E.M. 
##                                                 2 
##                                    Ralph Tresvant 
##                                                 1 
##                                           Ramones 
##                                                28 
##                                            Rancid 
##                                                31 
##                                     Rascal Flatts 
##                                                 2 
##                                 Ray J & Yung Berg 
##                                                 1 
##                                        Real McCoy 
##                                                 2 
##                                              Reba 
##                                                10 
##                             Red Hot Chili Peppers 
##                                                16 
##                                 Red Light Company 
##                                                10 
##                                     Reel Big Fish 
##                                                 5 
##                        Reggie and the Full Effect 
##                                                12 
##                                         Relient K 
##                                                19 
##                                          Rev. Run 
##                                                 8 
##                                        Ric Ocasek 
##                                                11 
##                                          Rich Boy 
##                                                 1 
##                                      Richard Marx 
##                                                12 
##                                       Rick Astley 
##                                                 1 
##                                  Rickie Lee Jones 
##                                                 7 
##                                      Ricky Martin 
##                                                12 
##                                           Rihanna 
##                                                11 
##                               Rihanna & Sean Paul 
##                                                 1 
##                                        Rob Thomas 
##                                                 1 
##                                        Rob Zombie 
##                                                26 
##                                      Robert Miles 
##                                                 8 
##                                          Robin S. 
##                                                 1 
##                                             Robyn 
##                                                 2 
##                                       Rod Stewart 
##                                                 5 
##                     Roger Miret and the Disasters 
##                                                11 
##                                        Rogue Wave 
##                                                12 
##                                              Rome 
##                                                 1 
##                                      Ron Sexsmith 
##                                                25 
##                                           Roxette 
##                                                 3 
##                                       Roy Orbison 
##                                                 2 
##                                    Ruben Studdard 
##                                                 2 
##                                         Ruff Endz 
##                                                 1 
##                                  Rufus Wainwright 
##                                                26 
##                                              Rush 
##                                                 8 
##                                        Ryan Adams 
##                                                12 
##                                      Ryan Malcolm 
##                                                14 
##                                               RZA 
##                                                24 
##                                          S Club 7 
##                                                 1 
##                                              Sade 
##                                                20 
##                                            Saliva 
##                                                10 
##                                       Salt-N-Pepa 
##                                                 2 
##                                       Sam Roberts 
##                                                 6 
##                                    Samantha Mumba 
##                                                 1 
##                                           Santana 
##                                                14 
##                                            Saosin 
##                                                13 
##                                    Sara Bareilles 
##                                                14 
##                                      Sarah Harmer 
##                                                11 
##                                      Sarah Hudson 
##                                                 1 
##                                   Sarah McLachlan 
##                                                 2 
##                                     Savage Garden 
##                                                 3 
##                                         Scorpions 
##                                                12 
##                                           Seabird 
##                                                12 
##                                              Seal 
##                                                 1 
##                                     Sean Kingston 
##                                                 3 
##                                         Sean Paul 
##                                                23 
##                                         Seduction 
##                                                 1 
##                                            Selena 
##                                                 9 
##                                            Shaggy 
##                                                 2 
##                                              Shai 
##                                                 2 
##                                           Shakira 
##                                                 2 
##                                      Shania Twain 
##                                                 2 
##                                           Shanice 
##                                                 1 
##                                      Shawn Colvin 
##                                                11 
##                                     Shawn Mullins 
##                                                 1 
##                                        Shearwater 
##                                                10 
##                                       Sheryl Crow 
##                                                 3 
##                                         Shinedown 
##                                                 1 
##                                         Shop Boyz 
##                                                 1 
##                                   Shout Out Louds 
##                                                 9 
##                                           Shwayze 
##                                                12 
##                                      Sick Puppies 
##                                                12 
##                                              Silk 
##                                                 1 
##                                 Silkk the Shocker 
##                                                18 
##                                       Silverchair 
##                                                10 
##                                       Silverstein 
##                                                 3 
##                                      Simple Minds 
##                                                15 
##                                           Sinergy 
##                                                 8 
##                                     Sir Mix-A-Lot 
##                                                 1 
##                                             Sisqo 
##                                                 2 
##                                    Sister Souljah 
##                                                 2 
##                          Sixpence None The Richer 
##                                                 1 
##                                          Skid Row 
##                                                 1 
##                                      Skinny Puppy 
##                                                10 
##                                    Slaughterhouse 
##                                                11 
##                                       Smash Mouth 
##                                                 1 
##                                              Smog 
##                                                 4 
##                                              Snap 
##                                                 2 
##                                        Snoop Dogg 
##                                                 4 
##                                              Snow 
##                                                 1 
##                                       Snow Patrol 
##                                                 1 
##                                       So They Say 
##                                                11 
##                                          Soilwork 
##                                                18 
##                          Somethin' For The People 
##                                                 1 
##                                           Sonique 
##                                                 1 
##                                 Sophie B. Hawkins 
##                                                 2 
##                                       Soul Asylum 
##                                                 1 
##                                     Soul For Real 
##                                                 1 
##                                      Soul II Soul 
##                                                 9 
##                                      soulDecision 
##                                                11 
##                                           Soulfly 
##                                                11 
##                                        Soulja Boy 
##                                                12 
##                               Soulja Boy Tell 'em 
##                                                 1 
##                                            Sparks 
##                                                 8 
##                                       Spice Girls 
##                                                 4 
##                                      Spin Doctors 
##                                                11 
##                                Spirit of the West 
##                                                10 
##                                     Spiritualized 
##                                                11 
##                                            Staind 
##                                                 1 
##                                          Static-X 
##                                                12 
##                                       Steve Earle 
##                                                10 
##                                          Stevie B 
##                                                 1 
##                                             Sting 
##                                                 1 
##                               Stone Temple Pilots 
##                                                13 
##                               Strapping Young Lad 
##                                                17 
##                                            Styles 
##                                                17 
##                                          Styles P 
##                                                 6 
##                                              Styx 
##                                                 1 
##                                         Submersed 
##                                                12 
##                                             Sugar 
##                                                20 
##                                         Sugar Ray 
##                                                15 
##                               Suicidal Tendencies 
##                                                21 
##                                      Super Junior 
##                                                 3 
##                                        Superchunk 
##                                                11 
##                                           Surface 
##                                                 1 
##                                             Swans 
##                                                15 
##                                   Sweet Sensation 
##                                                 1 
##                                               SWV 
##                                                 2 
##                                      Sylk-E. Fyne 
##                                                 1 
##                                            T-Pain 
##                                                 5 
##                                          t.A.T.u. 
##                                                 2 
##                                              T.I. 
##                                                 6 
##                                          Tag Team 
##                                                 1 
##                                         Taio Cruz 
##                                                 2 
##                                         Take That 
##                                                 1 
##                                          Talisman 
##                                                 9 
##                                             Tamia 
##                                                 1 
##                                           Taproot 
##                                                10 
##                                         Tara Kemp 
##                                                 2 
##                                       Tatyana Ali 
##                                                 2 
##                                      Taylor Dayne 
##                                                 3 
##                                      Taylor Hicks 
##                                                 1 
##                                      Taylor Swift 
##                                                 9 
##                                      Technotronic 
##                                                 3 
##                                   Teenage Fanclub 
##                                                13 
##                                      Terror Squad 
##                                                 1 
##                                             Tesla 
##                                                 2 
##                                    Tevin Campbell 
##                                                 2 
##                                             Texas 
##                                                13 
##                  Thao with the Get Down Stay Down 
##                                                 9 
##                          The All-American Rejects 
##                                                 3 
##                              The Apples in Stereo 
##                                                28 
##                                      The Audition 
##                                                 7 
##                                        The B-52 s 
##                                                 1 
##                                    The Beach Boys 
##                                                13 
##                                       The Beatles 
##                                                 1 
##                              The Bird and the Bee 
##                                                 9 
##                               The Black Eyed Peas 
##                                                19 
##                                          The Blow 
##                                                 9 
##                                The Bouncing Souls 
##                                                 7 
##                                       The Calling 
##                                                 1 
##                                    The Carpenters 
##                                                19 
##                             The Chemical Brothers 
##                                                 9 
##                                         The Cliks 
##                                                10 
##                                   The Cover Girls 
##                                                 1 
##                                   The Cranberries 
##                                                 1 
##                                          The Cult 
##                                                10 
##                                          The Cure 
##                                                12 
##                                  The Dead Milkmen 
##                                                12 
##                                            The Ex 
##                                                12 
##                                     The Exploited 
##                                                12 
##                                  The Flower Kings 
##                                                 5 
##                                          The Fray 
##                                                 3 
##                                          The Game 
##                                                21 
##                                     The Gathering 
##                                                19 
##                                       The Heights 
##                                                 1 
##                                   The Hold Steady 
##                                                 7 
##                             The Innocence Mission 
##                                                11 
##                                           The Jam 
##                                                26 
##                                      The Jayhawks 
##                                                14 
##                                  The Juan MacLean 
##                                                10 
##                                       The Killers 
##                                                10 
##                                           The KLF 
##                                                 1 
##                                          The La's 
##                                                12 
##                                    The Lemonheads 
##                                                10 
##                               The Magnetic Fields 
##                                                13 
##                       The Mighty Mighty Bosstones 
##                                                10 
##                                       The Mission 
##                                                10 
##                             The New Pornographers 
##                                                12 
##                              The Notorious B.I.G. 
##                                                 3 
##                                The Operation M.D. 
##                                                11 
##                                The Pete Best Band 
##                                                11 
##                                        The Police 
##                                                15 
##                              The Polyphonic Spree 
##                                                 9 
##    The Presidents of the United States of America 
##                                                10 
##                                The Pussycat Dolls 
##                                                 4 
##                        The Red Jumpsuit Apparatus 
##                                                11 
##                                     The Residents 
##                                                31 
##                                The Rolling Stones 
##                                                21 
##                                         The Roots 
##                                                13 
##                                  The Sea and Cake 
##                                                 7 
##                             The Smashing Pumpkins 
##                                                17 
##                                    The Stranglers 
##                                                18 
##                                       The Streets 
##                                                13 
##                                     The Tea Party 
##                                                11 
##                                          The Time 
##                                                 1 
##                             The Tony Rich Project 
##                                                 1 
##                                The Tragically Hip 
##                                                11 
##                                    The Verve Pipe 
##                                                 1 
##                                         The Vines 
##                                                 9 
##                                    The Wildhearts 
##                                                10 
##                                        The Zutons 
##                                                11 
##                                          Therapy? 
##                                                12 
##                              They Might Be Giants 
##                                                23 
##                                         Third Day 
##                                                12 
##                                   Third Eye Blind 
##                                                15 
##                                      This Is Hell 
##                                                13 
##                                    Throwing Muses 
##                                                10 
##                                          Thursday 
##                                                 3 
##                                        Tim McGraw 
##                                                 1 
##                                         Timbaland 
##                                                 3 
##                                          Timmy T. 
##                                                 1 
##                                               TLC 
##                                                 8 
##                                        Toby Keith 
##                                                10 
##                                      Tom Cochrane 
##                                                 1 
##                                         Tom Petty 
##                                                 1 
##                   Tom Petty and the Heartbreakers 
##                                                11 
##                                           Tom Vek 
##                                                10 
##                                      Toni Braxton 
##                                                 4 
##                                    Tony Toni Tone 
##                                                 1 
##                                 Tony! Toni! Ton_! 
##                                                14 
##                                         Tori Amos 
##                                                41 
##                                             Total 
##                                                 1 
##                                    Tracie Spencer 
##                                                 1 
##                                     Tracy Chapman 
##                                                 1 
##                                             Train 
##                                                 1 
##                          Trans-Siberian Orchestra 
##                                                20 
##                                       Transplants 
##                                                 9 
##                                             Trapt 
##                                                10 
##                                      Travie McCoy 
##                                                 1 
##                                      Travis Tritt 
##                                                 8 
##                                    Treble Charger 
##                                                10 
##                                        Trey Songz 
##                                                 1 
##                                       Trick Daddy 
##                                                14 
##                                        Trick Pony 
##                                                12 
##                                           Triumph 
##                                                 7 
##                                       Truth Hurts 
##                                                 1 
##                                             Tweet 
##                                                 1 
##                                            Twista 
##                                                 2 
##                                     Tyler Collins 
##                                                 1 
##                                            Tyrese 
##                                                 1 
##                                                U2 
##                                                24 
##                                              UB40 
##                                                 2 
##                                      Ugly Kid Joe 
##                                                 1 
##                                   Umphrey's McGee 
##                                                10 
##                                     Uncle Kracker 
##                                                 2 
##                                         Uncle Sam 
##                                                 1 
##                                               Unk 
##                                                 1 
##                                             Usher 
##                                                26 
##                               Usher & Alicia Keys 
##                                                 1 
##                             Usher And Alicia Keys 
##                                                 1 
##                                             Utada 
##                                                14 
##                                          Valencia 
##                                                 9 
##                                         Van Halen 
##                                                11 
##                                   Vanessa Carlton 
##                                                 1 
##                                  Vanessa Williams 
##                                                 1 
##                                       Vanilla Ice 
##                                                 2 
##                                           Various 
##                                                 9 
##                                   various artists 
##                                                10 
##                                   Various artists 
##                                               162 
##                                   Various Artists 
##                                                44 
##                                  Vertical Horizon 
##                                                 1 
##                                        Vice Squad 
##                                                13 
##                                    Violent Femmes 
##                                                15 
##                               Visions of Atlantis 
##                                                10 
##                                  Voices Of Theory 
##                                                 1 
##                                Voodoo Glow Skulls 
##                                                12 
##                                              VOTA 
##                                                11 
##                                           Warrant 
##                                                15 
##                                          Warren G 
##                                                 1 
##                                            Webbie 
##                                                 1 
##                                            Weezer 
##                                                 1 
##                                 Weird Al Yankovic 
##                                                 1 
##                              Wellwater Conspiracy 
##                                                10 
##                                   Whitney Houston 
##                                                14 
##                                             Wilco 
##                                                 2 
##                                        Will Smith 
##                                                 3 
##                                         will.i.am 
##                                                14 
##                                     Willie Nelson 
##                                                10 
##                                       Willie Nile 
##                                                13 
##                                   Wilson Phillips 
##                                                 3 
##                                          Witchery 
##                                                11 
##                                      Wooden Stars 
##                                                 7 
##                                             Woods 
##                                                11 
##                                   Wreckx-N-Effect 
##                                                 1 
##                                       Wyclef Jean 
##                                                19 
##                                            X-Clan 
##                                                 6 
##                                            Xscape 
##                                                 6 
##                                               XTC 
##                                                16 
##                                               Y&T 
##                                                12 
##                                         Yael Naim 
##                                                 1 
##                                Year of the Rabbit 
##                                                11 
##                                               Yes 
##                                                 7 
##                                   Ying Yang Twins 
##                                                 1 
##                                  Yngwie Malmsteen 
##                                                10 
##                                         Young Dro 
##                                                 1 
##                                       Young Jeezy 
##                                                 1 
##                                       Young Money 
##                                                 2 
##                                       YoungBloodZ 
##                                                 1 
##                                          Yung Joc 
##                                                 1 
##                                         Zebrahead 
##                                                14 
##                                            Zero 7 
##                                                11 
##                                             Zhane 
##                                                 1
```

```r
print(subset(glb_entity_df, artistname == "Michael Jackson")
      [, c("artistname", "songtitle", "Top10")])
```

```
##           artistname                      songtitle Top10
## 4329 Michael Jackson              You Rock My World     1
## 6205 Michael Jackson           She's Out of My Life     0
## 6206 Michael Jackson    Wanna Be Startin' Somethin'     0
## 6207 Michael Jackson              You Are Not Alone     1
## 6208 Michael Jackson                    Billie Jean     0
## 6209 Michael Jackson       The Way You Make Me Feel     0
## 6210 Michael Jackson                 Black or White     1
## 6211 Michael Jackson                  Rock with You     0
## 6212 Michael Jackson                            Bad     0
## 6213 Michael Jackson   I Just Can't Stop Loving You     0
## 6214 Michael Jackson              Man in the Mirror     0
## 6215 Michael Jackson                       Thriller     0
## 6216 Michael Jackson                        Beat It     0
## 6217 Michael Jackson               The Girl Is Mine     0
## 6218 Michael Jackson              Remember the Time     1
## 6219 Michael Jackson Don't Stop 'Til You Get Enough     0
## 6220 Michael Jackson                 Heal the World     0
## 6915 Michael Jackson                  In The Closet     1
```

```r
print(table(glb_entity_df$timesignature))
```

```
## 
##    0    1    3    4    5    7 
##   10  143  503 6787  112   19
```

```r
print(glb_entity_df[which.max(glb_entity_df$tempo), ])
```

```
##      year                   songtitle      artistname             songID
## 6206 1995 Wanna Be Startin' Somethin' Michael Jackson SONHIQM13738B7BE80
##                artistID timesignature timesignature_confidence loudness
## 6206 ARXPPEY1187FB51DF4             3                        1  -14.528
##        tempo tempo_confidence key key_confidence    energy pitch
## 6206 244.307            0.566   6           0.44 0.6379941 0.009
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 6206        11.84       44.378        -80.4      187.038      -106.47
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 6206      220.751      -79.722      199.699      -77.158      147.564
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 6206      -68.229      186.391     -110.029       63.148      -58.978
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 6206         93.6      -52.012       95.827      -63.554       84.129
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 6206       -53.492        67.001       -73.421        67.308     0
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="<newdt_url>", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_entity_df, parse(text=glb_split_newdata_condition)))
            glb_entity_df <- do.call("subset", 
                list(glb_entity_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_split_newdata_size))
                glb_newent_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
```

```
##   year                          songtitle        artistname
## 1 2010 This Is the House That Doubt Built A Day to Remember
## 2 2010                    Sticks & Bricks A Day to Remember
## 3 2010                         All I Want A Day to Remember
## 4 2010                   It's Complicated A Day to Remember
## 5 2010                          2nd Sucks A Day to Remember
## 6 2010                Better Off This Way A Day to Remember
##               songID           artistID timesignature
## 1 SOBGGAB12C5664F054 AROBSHL1187B9AFB01             3
## 2 SOPAQHU1315CD47F31 AROBSHL1187B9AFB01             4
## 3 SOOIZOU1376E7C6386 AROBSHL1187B9AFB01             4
## 4 SODRYWD1315CD49DBE AROBSHL1187B9AFB01             4
## 5 SOICMQB1315CD46EE3 AROBSHL1187B9AFB01             4
## 6 SOCEYON1315CD4A23E AROBSHL1187B9AFB01             4
##   timesignature_confidence loudness   tempo tempo_confidence key
## 1                    0.853   -4.262  91.525            0.953  11
## 2                    1.000   -4.051 140.048            0.921  10
## 3                    1.000   -3.571 160.512            0.489   2
## 4                    1.000   -3.815  97.525            0.794   1
## 5                    0.788   -4.707 140.053            0.286   6
## 6                    1.000   -3.807 160.366            0.347   4
##   key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 1          0.453 0.9666556 0.024        0.002       57.342       -6.496
## 2          0.469 0.9847095 0.025        0.000       57.414      -37.351
## 3          0.209 0.9899004 0.026        0.003       57.422      -17.222
## 4          0.632 0.9392072 0.013        0.000       57.765      -32.083
## 5          0.483 0.9877376 0.063        0.000       56.872     -223.922
## 6          0.627 0.9799530 0.038        0.000       57.083      -40.408
##   timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 1      171.093      -81.664       95.117     -285.049      259.426
## 2      171.130     -149.589      180.334     -380.143      384.166
## 3      171.060      -72.912      157.925     -203.984      251.258
## 4      220.895     -138.596      173.365      -73.490      373.492
## 5      171.130     -147.153      166.008     -128.082      389.419
## 6      174.002      -83.829      126.879     -100.119      173.607
##   timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 1      -40.385       73.630     -104.683      183.089      -88.771
## 2      -48.662      100.414      -87.267       42.798      -86.895
## 3      -66.044      152.095      -98.673      141.365      -88.874
## 4      -55.607      119.158      -77.515      141.178      -70.790
## 5      -43.908       99.332      -96.147       38.303     -110.757
## 6      -33.789       66.904      -84.451       47.268      -71.219
##   timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 1       73.549      -71.127       82.475      -52.025       39.116
## 2       75.455      -65.807      106.918      -61.320       35.378
## 3       66.504      -67.433       80.621      -59.773       45.979
## 4       64.540      -63.667       96.675      -78.660       41.088
## 5       72.391      -55.935      110.332      -56.450       37.555
## 6       71.239      -79.948       91.117      -54.378       53.808
##   timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 1      -35.368       71.642      -126.440        18.658       -44.770
## 2      -81.928       74.574      -103.808       121.935       -38.892
## 3      -46.293       59.904      -108.313        33.300       -43.733
## 4      -49.194       95.440      -102.676        46.422       -59.439
## 5      -48.588       67.570       -52.796        22.888       -50.414
## 6      -33.183       54.657       -64.478        34.522       -40.922
##   timbre_11_max Top10
## 1        25.989     0
## 2        22.513     0
## 3        25.744     0
## 4        37.082     0
## 5        32.758     0
## 6        36.453     0
##     year           songtitle           artistname             songID
## 62  2010 More Than Worthless        Drowning Pool SOEZTPW12AB0186736
## 169 2010        Need You Now      Lady Antebellum SOKLNZZ12AB0189411
## 269 2010                Hard              Rihanna SOLSQLC13731ABDE72
## 283 2010         God Made Me           Shearwater SOVPZJL12AB01899B4
## 326 2010     Kiss on My List The Bird and the Bee SOBXVFZ13167714C29
## 328 2010          She's Gone The Bird and the Bee SOZGSXE12AB0185C68
##               artistID timesignature timesignature_confidence loudness
## 62  ARVSQHB1187B992FF0             4                    1.000   -4.492
## 169 ARFJXKQ119B86694AF             4                    0.990   -5.018
## 269 ARKU3Z61187FB51DCA             4                    1.000   -5.973
## 283 ARM6D9O1187B9ABC29             1                    0.205  -13.333
## 326 AR2JB471187FB470A2             4                    1.000   -9.412
## 328 AR2JB471187FB470A2             4                    1.000  -11.371
##       tempo tempo_confidence key key_confidence    energy pitch
## 62   83.498            0.000   9          0.058 0.9251590 0.005
## 169 107.907            0.979   4          0.571 0.6776538 0.009
## 269 130.054            0.894  11          0.684 0.8815718 0.006
## 283  82.044            0.118   7          0.541 0.1881568 0.001
## 326 119.985            1.000   7          0.255 0.6724729 0.001
## 328  77.993            0.022   9          0.469 0.4364502 0.001
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 62         0.002       57.799     -130.567      260.456     -157.457
## 169        8.749       58.470     -137.899      176.853     -141.300
## 269        0.349       56.536     -106.587      281.821     -187.247
## 283        0.779       54.475     -167.523      191.877      -92.049
## 326        0.000       54.922     -121.498      179.794     -214.082
## 328        0.000       51.426     -226.861      219.188     -200.666
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 62        64.597     -273.879       85.221      -74.961       67.258
## 169      127.053     -220.501      134.982      -64.337      117.601
## 269      226.528     -250.505      273.720      -74.642      127.575
## 283      175.565     -118.525       94.512      -83.220      116.127
## 326      120.225     -196.055      156.001      -66.275      115.281
## 328      222.564     -191.963      138.612      -69.949      109.030
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 62       -91.914      224.704     -105.666       28.787      -84.908
## 169      -90.842       85.592      -61.944       78.253      -77.873
## 269     -154.622      141.457      -74.038       95.873     -109.799
## 283      -91.340       50.307      -70.923       69.920      -92.368
## 326     -115.838      165.724      -94.499       89.203     -109.323
## 328     -126.186      137.382     -124.355      140.768     -102.701
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 62       114.744      -69.266       64.859      -42.872       90.805
## 169      103.718      -66.331       47.192      -66.833       64.273
## 269      100.552      -62.639       54.530      -85.890       82.944
## 283      124.275      -85.608       49.650      -49.102       44.755
## 326       88.023      -42.074       65.908     -104.575      101.912
## 328       91.823      -68.029       62.160      -62.990       64.775
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 62        -54.512        76.567       -49.614        29.581     0
## 169       -56.881        44.590       -54.487        40.020     1
## 269      -160.967        50.229       -66.591        66.126     1
## 283       -49.056        38.846       -45.282        60.022     0
## 326      -103.161        53.649       -49.239        47.821     0
## 328       -70.405        60.822       -47.582        75.628     0
##     year            songtitle      artistname             songID
## 368 2010       Mighty to Save Various Artists SOZHGUW137375147FE
## 369 2010 All Because of Jesus Various Artists SOQNGML13DC8B9A8A5
## 370 2010            Your Name Various Artists SOXGDEA1375A1AD2F8
## 371 2010            You Reign Various Artists SOEHNSK13DC4A18344
## 372 2010     New Song We Sing Various Artists SOQOCRO13DC469AD34
## 373 2010              BedRock     Young Money SOPQHEH1374011CFB8
##               artistID timesignature timesignature_confidence loudness
## 368 ARAGWS81187FB3F768             4                    0.465  -11.768
## 369 ARAGWS81187FB3F768             4                    0.851   -6.896
## 370 ARAGWS81187FB3F768             4                    0.937   -7.695
## 371 ARAGWS81187FB3F768             4                    0.581  -13.761
## 372 ARAGWS81187FB3F768             4                    0.869  -12.884
## 373 ARFNQBX122BCFCC40F             4                    0.230   -5.860
##       tempo tempo_confidence key key_confidence    energy pitch
## 368 147.962            0.455   6          0.715 0.7076861 0.004
## 369 132.014            0.897   5          0.593 0.8218041 0.002
## 370  79.990            0.345   3          0.512 0.5038695 0.003
## 371  75.019            0.171   2          0.689 0.3286537 0.002
## 372 120.074            0.710   9          0.664 0.3213989 0.002
## 373 148.328            0.266   8          0.458 0.7864918 0.005
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 368       13.320       51.565     -112.753      190.046     -109.409
## 369        0.000       55.832     -253.356      171.130     -113.093
## 370        0.008       57.291     -137.997      182.508     -108.564
## 371        0.000       52.977     -120.113      179.066      -99.847
## 372        0.000       50.539     -160.382      186.927     -135.012
## 373       33.782       56.072      -40.255      309.536     -131.923
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 368      118.560     -144.309      125.791      -54.146      132.050
## 369      108.598     -152.949       94.648      -50.066      142.013
## 370      100.557     -230.832       74.961      -40.792       94.952
## 371      143.332     -209.714      131.918      -37.509      132.303
## 372      196.199     -182.354      229.050      -33.858      128.405
## 373      126.088     -159.948      169.983      -54.751      148.642
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 368      -88.464       77.963      -69.467       69.589      -58.550
## 369      -80.187       42.780      -53.975       70.978     -113.356
## 370      -95.304      157.062      -84.841       73.270      -92.767
## 371      -86.539      176.159      -85.282       96.528     -123.525
## 372      -87.455      158.989     -135.819       69.098      -94.179
## 373     -111.185      125.831     -111.483       72.581      -73.553
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 368       86.250      -72.253       61.809      -55.720       66.168
## 369      122.253      -50.909       61.278      -65.765       61.128
## 370       91.915      -78.383       29.942      -62.093       47.771
## 371      100.910     -100.195       50.555      -64.052       60.195
## 372       99.038      -53.112       42.895      -64.875       99.467
## 373       88.488      -70.974       51.550      -51.106       85.348
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 368       -63.432        44.960       -61.604        48.130     0
## 369       -54.461        32.120       -46.549        32.486     0
## 370       -49.670        51.155       -44.724        41.997     0
## 371       -83.234        51.323       -59.022        63.993     0
## 372      -111.697        49.754       -39.289        69.057     0
## 373       -84.959        74.872       -57.014        51.651     1
## 'data.frame':	373 obs. of  39 variables:
##  $ year                    : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
##  $ songtitle               : chr  "This Is the House That Doubt Built" "Sticks & Bricks" "All I Want" "It's Complicated" ...
##  $ artistname              : chr  "A Day to Remember" "A Day to Remember" "A Day to Remember" "A Day to Remember" ...
##  $ songID                  : chr  "SOBGGAB12C5664F054" "SOPAQHU1315CD47F31" "SOOIZOU1376E7C6386" "SODRYWD1315CD49DBE" ...
##  $ artistID                : chr  "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" ...
##  $ timesignature           : int  3 4 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.853 1 1 1 0.788 1 0.968 0.861 0.622 0.938 ...
##  $ loudness                : num  -4.26 -4.05 -3.57 -3.81 -4.71 ...
##  $ tempo                   : num  91.5 140 160.5 97.5 140.1 ...
##  $ tempo_confidence        : num  0.953 0.921 0.489 0.794 0.286 0.347 0.273 0.83 0.018 0.929 ...
##  $ key                     : int  11 10 2 1 6 4 10 5 9 11 ...
##  $ key_confidence          : num  0.453 0.469 0.209 0.632 0.483 0.627 0.715 0.423 0.751 0.602 ...
##  $ energy                  : num  0.967 0.985 0.99 0.939 0.988 ...
##  $ pitch                   : num  0.024 0.025 0.026 0.013 0.063 0.038 0.026 0.033 0.027 0.004 ...
##  $ timbre_0_min            : num  0.002 0 0.003 0 0 ...
##  $ timbre_0_max            : num  57.3 57.4 57.4 57.8 56.9 ...
##  $ timbre_1_min            : num  -6.5 -37.4 -17.2 -32.1 -223.9 ...
##  $ timbre_1_max            : num  171 171 171 221 171 ...
##  $ timbre_2_min            : num  -81.7 -149.6 -72.9 -138.6 -147.2 ...
##  $ timbre_2_max            : num  95.1 180.3 157.9 173.4 166 ...
##  $ timbre_3_min            : num  -285 -380.1 -204 -73.5 -128.1 ...
##  $ timbre_3_max            : num  259 384 251 373 389 ...
##  $ timbre_4_min            : num  -40.4 -48.7 -66 -55.6 -43.9 ...
##  $ timbre_4_max            : num  73.6 100.4 152.1 119.2 99.3 ...
##  $ timbre_5_min            : num  -104.7 -87.3 -98.7 -77.5 -96.1 ...
##  $ timbre_5_max            : num  183.1 42.8 141.4 141.2 38.3 ...
##  $ timbre_6_min            : num  -88.8 -86.9 -88.9 -70.8 -110.8 ...
##  $ timbre_6_max            : num  73.5 75.5 66.5 64.5 72.4 ...
##  $ timbre_7_min            : num  -71.1 -65.8 -67.4 -63.7 -55.9 ...
##  $ timbre_7_max            : num  82.5 106.9 80.6 96.7 110.3 ...
##  $ timbre_8_min            : num  -52 -61.3 -59.8 -78.7 -56.5 ...
##  $ timbre_8_max            : num  39.1 35.4 46 41.1 37.6 ...
##  $ timbre_9_min            : num  -35.4 -81.9 -46.3 -49.2 -48.6 ...
##  $ timbre_9_max            : num  71.6 74.6 59.9 95.4 67.6 ...
##  $ timbre_10_min           : num  -126.4 -103.8 -108.3 -102.7 -52.8 ...
##  $ timbre_10_max           : num  18.7 121.9 33.3 46.4 22.9 ...
##  $ timbre_11_min           : num  -44.8 -38.9 -43.7 -59.4 -50.4 ...
##  $ timbre_11_max           : num  26 22.5 25.7 37.1 32.8 ...
##  $ Top10                   : int  0 0 0 0 0 0 0 0 0 1 ...
##  - attr(*, "comment")= chr "glb_newent_df"
##     year              songtitle artistname             songID
## 374 2009    The Awkward Goodbye    Athlete SOUALGK12AB017FC37
## 375 2009           Rubik's Cube    Athlete SOGPIQC12AB0182B15
## 376 2009       Superhuman Touch    Athlete SOBNYZN13774E81F76
## 377 2009            The Getaway    Athlete SOHFEOA1366EE931DD
## 378 2009        Black Swan Song    Athlete SOXXSMX12AB017F7B3
## 379 2009 Don't Hold Your Breath    Athlete SOOEDWA12AB017FC13
##               artistID timesignature timesignature_confidence loudness
## 374 ARDW3YJ1187FB4CCE5             3                    0.732   -6.320
## 375 ARDW3YJ1187FB4CCE5             3                    0.906   -9.541
## 376 ARDW3YJ1187FB4CCE5             4                    0.987   -4.842
## 377 ARDW3YJ1187FB4CCE5             4                    0.822   -5.272
## 378 ARDW3YJ1187FB4CCE5             4                    0.983   -6.233
## 379 ARDW3YJ1187FB4CCE5             4                    1.000   -6.793
##       tempo tempo_confidence key key_confidence    energy pitch
## 374  89.614            0.652   1          0.773 0.5985294 0.004
## 375 117.742            0.542   0          0.722 0.3633990 0.006
## 376 119.018            0.838   6          0.106 0.7601515 0.003
## 377  71.479            0.613   4          0.781 0.7550336 0.014
## 378  77.492            0.740   8          0.552 0.5236583 0.008
## 379  81.859            0.821   9          0.218 0.6546091 0.012
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 374        0.000       57.831      -62.306      285.818      -81.802
## 375        0.739       57.059     -220.205      241.091      -96.833
## 376        0.000       57.815     -189.660      187.282     -139.053
## 377        0.000       58.330     -113.885      171.130      -71.640
## 378        0.000       57.643     -160.579      216.778      -79.456
## 379        0.000       57.389     -103.691      227.209     -155.016
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 374      211.084     -217.025      203.151      -55.874       97.646
## 375      214.510     -201.889      124.200      -52.389      131.859
## 376      134.508     -116.316       94.698      -55.617       79.292
## 377      194.788     -276.297      146.268      -59.374      121.707
## 378      114.093     -183.559      108.719      -31.922      169.734
## 379      174.758     -386.464      185.756      -69.700      103.106
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 374      -62.492       82.169      -82.129       59.197     -109.384
## 375      -73.875       73.628      -63.496       70.133      -90.092
## 376      -73.474       41.025      -41.489       62.759      -69.311
## 377      -71.135       39.607      -77.786       94.525      -69.088
## 378      -73.004      233.930      -76.026       58.016      -78.803
## 379      -75.267      184.309      -62.755       45.283      -61.878
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 374       70.975      -71.776       58.432      -53.816       88.571
## 375      112.879      -64.470       58.086      -76.937       74.441
## 376       90.400      -52.459       40.679      -50.408       58.811
## 377       93.373      -55.811       78.963      -51.504       70.455
## 378      100.766      -61.392       50.309      -62.994       96.837
## 379       89.443      -70.718       50.515      -54.980       80.278
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 374       -89.816        38.026       -52.075        52.827     0
## 375       -88.244        42.209       -66.812        40.749     0
## 376       -78.239        35.264       -54.200        46.490     0
## 377       -74.928        30.839       -51.377        27.768     0
## 378       -90.397        60.549       -52.122        48.059     0
## 379       -70.317        54.084       -43.651        43.261     0
##      year                songtitle         artistname             songID
## 622  2009               Our Window Noah and the Whale SOQJHVY12AB0185E10
## 1470 2007               Hood Nigga        Gorilla Zoe SOYXGHM1311AFE3CE0
## 2715 2005 If I Had Changed My Mind            Tom Vek SOEQLEO1311AFDD161
## 4040 2002    Get The Party Started               Pink SOBRRIE136F1FF93AA
## 5612 1997     Your Kid Is Deformed          Anal Cunt SOCNHOB1313439F2CB
## 7498 1990                  Feelin'           The La's SOIFMFD12A6701E0B7
##                artistID timesignature timesignature_confidence loudness
## 622  AR9ZC461187FB511F7             4                    0.968  -17.641
## 1470 ARX70TX1187FB53005             4                    0.927   -3.977
## 2715 ARCDWKV1187B98C4CD             4                    0.829   -4.998
## 4040 ARTOCVV12FE0871577             4                    1.000   -6.967
## 5612 ARL14X91187FB4CF14             4                    1.000  -10.810
## 7498 AR8V85J1187B9ACB4A             4                    1.000   -7.758
##        tempo tempo_confidence key key_confidence    energy pitch
## 622  115.953            0.322  10          0.914 0.1835704 0.003
## 1470  84.507            0.639   6          0.204 0.8595407 0.008
## 2715  73.147            0.127  11          0.351 0.9593842 0.009
## 4040 130.000            0.904   9          0.092 0.9048962 0.022
## 5612  82.712            0.500   4          0.000 0.9491671 0.125
## 7498  92.383            1.000   5          0.053 0.9654274 0.033
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 622         8.132       49.897     -196.811      221.505     -134.592
## 1470       43.578       57.339      -94.109      258.085      -92.694
## 2715        0.000       58.649      -88.568      224.086     -163.355
## 4040       24.695       54.237       -4.113      204.499     -149.133
## 5612        0.034       51.886       -1.922      234.119     -143.170
## 7498        0.000       53.631      -43.431      171.130     -109.124
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 622        83.178     -222.193      103.408      -76.284      102.323
## 1470      145.181     -137.955       89.956      -21.381      123.191
## 2715      132.388     -207.049      390.787      -63.571      100.651
## 4040      143.755     -167.463      366.443      -79.647      105.883
## 5612      182.408     -105.235      365.689      -77.538       58.212
## 7498      135.768      -95.871      365.709      -56.523       76.669
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 622       -85.203      122.616      -81.637       74.122      -85.814
## 1470      -86.896       58.584      -55.012       81.648      -68.723
## 2715      -80.796       99.606      -89.387       61.820      -59.098
## 4040     -223.656       85.211      -61.275       95.261      -62.953
## 5612      -59.521      121.803      -70.083       95.790      -22.254
## 7498      -91.293      258.697      -90.748       52.918      -54.980
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 622       109.135      -64.663       40.659      -78.981       76.208
## 1470       88.277      -63.306       57.092      -64.171       58.082
## 2715       81.037      -71.266       31.490      -52.055       53.340
## 4040       67.739      -66.865       33.560      -68.560       48.213
## 5612       98.110      -68.230       41.482      -71.967       37.290
## 7498      115.395      -41.211       48.446      -51.131       55.878
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 622        -85.839        50.949       -40.376        50.376     0
## 1470       -71.853        34.219       -45.129        56.355     0
## 2715      -144.111        47.682       -50.064        42.867     0
## 4040      -114.604        45.690       -71.931        51.818     1
## 5612       -87.556        34.307       -22.141        50.723     0
## 7498       -84.279        57.627       -44.116        27.300     0
##      year              songtitle artistname             songID
## 7569 1990        Red Hot & Ready        Y&T SOHVWXR12A6D4FC59C
## 7570 1990             She's Gone        Y&T SOSIEQB12A6D4FC59D
## 7571 1990             Let It Out        Y&T SOIGIQI12A6D4FC59E
## 7572 1990             Ten Lovers        Y&T SOLNEQO12A6D4FC59F
## 7573 1990 Goin' Off The Deep End        Y&T SOEMJEP12A58A7E7B5
## 7574 1990              Surrender        Y&T SOLBQSX12A6D4FC5A0
##                artistID timesignature timesignature_confidence loudness
## 7569 ARGQANQ11F50C4769E             4                    1.000  -10.574
## 7570 ARGQANQ11F50C4769E             4                    1.000  -10.197
## 7571 ARGQANQ11F50C4769E             4                    1.000  -12.392
## 7572 ARGQANQ11F50C4769E             4                    0.984  -10.304
## 7573 ARGQANQ11F50C4769E             4                    0.907   -9.295
## 7574 ARGQANQ11F50C4769E             4                    0.987   -9.762
##        tempo tempo_confidence key key_confidence    energy pitch
## 7569 137.872            1.000   2          0.006 0.9736458 0.030
## 7570  93.140            0.859   5          0.889 0.9429917 0.016
## 7571  79.858            0.196   9          0.149 0.8124216 0.012
## 7572  91.760            0.592   2          0.077 0.7368709 0.016
## 7573 110.907            0.838   9          0.621 0.9900532 0.061
## 7574 139.650            0.781   6          0.193 0.9448227 0.027
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 7569            0       51.847      -18.801      203.289     -141.762
## 7570            0       53.462      -22.878      202.424      -77.467
## 7571            0       51.354      -91.916      202.639      -76.736
## 7572            0       53.358      -10.087      202.877      -48.768
## 7573            0       52.928      -15.289      175.845     -119.044
## 7574            0       52.989      -32.907      195.601     -146.973
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 7569      157.204     -120.035      364.391      -53.240       99.468
## 7570      174.663     -170.802      201.677      -45.807      109.383
## 7571      165.643     -186.653      166.200      -63.367      132.495
## 7572      194.665     -201.008      132.174      -55.916      129.674
## 7573      164.155     -162.944      391.622      -42.728      137.159
## 7574      150.596      -79.356      307.651      -39.215       68.426
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 7569      -82.554      115.220      -91.407       62.922      -57.069
## 7570      -75.011      103.305     -130.215       59.775      -61.197
## 7571     -115.231       86.509      -83.905      102.373      -66.416
## 7572      -82.989      166.003      -83.246       62.951      -69.512
## 7573      -80.171       92.551      -64.419       74.428      -38.794
## 7574      -77.927      202.200      -55.617       98.615      -71.984
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 7569       96.283      -56.468       34.205      -50.403       51.369
## 7570       38.120      -66.638       50.105      -40.692       43.763
## 7571       83.454      -83.707       44.124      -51.628       69.929
## 7572      103.413     -101.464       36.152      -45.387       48.352
## 7573      108.688      -55.893       42.222      -76.631       68.336
## 7574       87.098      -48.440       52.198      -60.673       48.418
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 7569       -63.965        82.665       -47.429        58.158     0
## 7570       -59.707        49.414       -53.970        68.303     0
## 7571       -97.153        36.745       -61.243        56.902     0
## 7572       -57.103        67.641       -53.729        65.176     0
## 7573       -83.284        56.476       -51.687        59.427     0
## 7574      -120.625        49.593       -47.656        70.005     0
## 'data.frame':	7201 obs. of  39 variables:
##  $ year                    : int  2009 2009 2009 2009 2009 2009 2009 2009 2009 2009 ...
##  $ songtitle               : chr  "The Awkward Goodbye" "Rubik's Cube" "Superhuman Touch" "The Getaway" ...
##  $ artistname              : chr  "Athlete" "Athlete" "Athlete" "Athlete" ...
##  $ songID                  : chr  "SOUALGK12AB017FC37" "SOGPIQC12AB0182B15" "SOBNYZN13774E81F76" "SOHFEOA1366EE931DD" ...
##  $ artistID                : chr  "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" ...
##  $ timesignature           : int  3 3 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.732 0.906 0.987 0.822 0.983 1 0.821 0.997 0.816 1 ...
##  $ loudness                : num  -6.32 -9.54 -4.84 -5.27 -6.23 ...
##  $ tempo                   : num  89.6 117.7 119 71.5 77.5 ...
##  $ tempo_confidence        : num  0.652 0.542 0.838 0.613 0.74 0.821 0.912 0.609 0.786 0.27 ...
##  $ key                     : int  1 0 6 4 8 9 6 9 0 9 ...
##  $ key_confidence          : num  0.773 0.722 0.106 0.781 0.552 0.218 0.275 0.333 0.634 0.578 ...
##  $ energy                  : num  0.599 0.363 0.76 0.755 0.524 ...
##  $ pitch                   : num  0.004 0.006 0.003 0.014 0.008 0.012 0.002 0.003 0.001 0.006 ...
##  $ timbre_0_min            : num  0 0.739 0 0 0 ...
##  $ timbre_0_max            : num  57.8 57.1 57.8 58.3 57.6 ...
##  $ timbre_1_min            : num  -62.3 -220.2 -189.7 -113.9 -160.6 ...
##  $ timbre_1_max            : num  286 241 187 171 217 ...
##  $ timbre_2_min            : num  -81.8 -96.8 -139.1 -71.6 -79.5 ...
##  $ timbre_2_max            : num  211 215 135 195 114 ...
##  $ timbre_3_min            : num  -217 -202 -116 -276 -184 ...
##  $ timbre_3_max            : num  203.2 124.2 94.7 146.3 108.7 ...
##  $ timbre_4_min            : num  -55.9 -52.4 -55.6 -59.4 -31.9 ...
##  $ timbre_4_max            : num  97.6 131.9 79.3 121.7 169.7 ...
##  $ timbre_5_min            : num  -62.5 -73.9 -73.5 -71.1 -73 ...
##  $ timbre_5_max            : num  82.2 73.6 41 39.6 233.9 ...
##  $ timbre_6_min            : num  -82.1 -63.5 -41.5 -77.8 -76 ...
##  $ timbre_6_max            : num  59.2 70.1 62.8 94.5 58 ...
##  $ timbre_7_min            : num  -109.4 -90.1 -69.3 -69.1 -78.8 ...
##  $ timbre_7_max            : num  71 112.9 90.4 93.4 100.8 ...
##  $ timbre_8_min            : num  -71.8 -64.5 -52.5 -55.8 -61.4 ...
##  $ timbre_8_max            : num  58.4 58.1 40.7 79 50.3 ...
##  $ timbre_9_min            : num  -53.8 -76.9 -50.4 -51.5 -63 ...
##  $ timbre_9_max            : num  88.6 74.4 58.8 70.5 96.8 ...
##  $ timbre_10_min           : num  -89.8 -88.2 -78.2 -74.9 -90.4 ...
##  $ timbre_10_max           : num  38 42.2 35.3 30.8 60.5 ...
##  $ timbre_11_min           : num  -52.1 -66.8 -54.2 -51.4 -52.1 ...
##  $ timbre_11_max           : num  52.8 40.7 46.5 27.8 48.1 ...
##  $ Top10                   : int  0 0 0 0 0 0 0 0 0 0 ...
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# year = the year the song was released
# songtitle = the title of the song
# artistname = the name of the artist of the song
# songID and artistID = identifying variables for the song and artist
# timesignature and timesignature_confidence = a variable estimating the time signature of the song, and the confidence in the estimate
# loudness = a continuous variable indicating the average amplitude of the audio in decibels
# tempo and tempo_confidence = a variable indicating the estimated beats per minute of the song, and the confidence in the estimate
# key and key_confidence = a variable with twelve levels indicating the estimated key of the song (C, C#, . . ., B), and the confidence in the estimate
# energy = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudness
# pitch = a continuous variable that indicates the pitch of the song
# timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, . . . , timbre_11_min, and timbre_11_max = variables that indicate the minimum/maximum values over all segments for each of the twelve values in the timbre vector (resulting in 24 continuous variables)
# Top10 = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 if it was in the top 10, and 0 if it was not)

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))) 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>)        
        .rnorm=rnorm(1)
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##       year       songtitle          artistname           songID         
##  Min.   :1990   Length:7201        Length:7201        Length:7201       
##  1st Qu.:1997   Class :character   Class :character   Class :character  
##  Median :2002   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2001                                                           
##  3rd Qu.:2006                                                           
##  Max.   :2009                                                           
##    artistID         timesignature   timesignature_confidence
##  Length:7201        Min.   :0.000   Min.   :0.0000          
##  Class :character   1st Qu.:4.000   1st Qu.:0.8200          
##  Mode  :character   Median :4.000   Median :0.9800          
##                     Mean   :3.894   Mean   :0.8533          
##                     3rd Qu.:4.000   3rd Qu.:1.0000          
##                     Max.   :7.000   Max.   :1.0000          
##     loudness           tempo        tempo_confidence      key        
##  Min.   :-42.451   Min.   :  0.00   Min.   :0.0000   Min.   : 0.000  
##  1st Qu.:-10.915   1st Qu.: 88.95   1st Qu.:0.3730   1st Qu.: 2.000  
##  Median : -7.732   Median :103.04   Median :0.7060   Median : 6.000  
##  Mean   : -8.887   Mean   :107.22   Mean   :0.6248   Mean   : 5.381  
##  3rd Qu.: -5.723   3rd Qu.:124.46   3rd Qu.:0.8930   3rd Qu.: 9.000  
##  Max.   :  1.305   Max.   :244.31   Max.   :1.0000   Max.   :11.000  
##  key_confidence       energy            pitch          timbre_0_min   
##  Min.   :0.0000   Min.   :0.00002   Min.   :0.00000   Min.   : 0.000  
##  1st Qu.:0.2060   1st Qu.:0.49766   1st Qu.:0.00300   1st Qu.: 0.000  
##  Median :0.4520   Median :0.71563   Median :0.00700   Median : 0.027  
##  Mean   :0.4351   Mean   :0.67344   Mean   :0.01084   Mean   : 4.052  
##  3rd Qu.:0.6470   3rd Qu.:0.88593   3rd Qu.:0.01400   3rd Qu.: 2.626  
##  Max.   :1.0000   Max.   :0.99849   Max.   :0.54100   Max.   :48.353  
##   timbre_0_max    timbre_1_min      timbre_1_max     timbre_2_min    
##  Min.   :12.58   Min.   :-333.72   Min.   :-74.37   Min.   :-324.86  
##  1st Qu.:53.05   1st Qu.:-159.43   1st Qu.:171.13   1st Qu.:-168.00  
##  Median :55.48   Median :-107.67   Median :194.39   Median :-136.80  
##  Mean   :54.41   Mean   :-110.45   Mean   :212.32   Mean   :-137.18  
##  3rd Qu.:57.04   3rd Qu.: -59.39   3rd Qu.:239.34   3rd Qu.:-106.74  
##  Max.   :64.01   Max.   : 123.73   Max.   :549.97   Max.   :  34.57  
##   timbre_2_max      timbre_3_min      timbre_3_max     timbre_4_min    
##  Min.   : -0.832   Min.   :-495.36   Min.   : 12.85   Min.   :-207.07  
##  1st Qu.: 99.969   1st Qu.:-226.62   1st Qu.:127.11   1st Qu.: -78.05  
##  Median :129.373   Median :-170.55   Median :189.48   Median : -63.97  
##  Mean   :136.400   Mean   :-185.89   Mean   :211.55   Mean   : -65.50  
##  3rd Qu.:165.844   3rd Qu.:-131.73   3rd Qu.:289.40   3rd Qu.: -51.52  
##  Max.   :397.095   Max.   : -21.55   Max.   :499.62   Max.   :  51.43  
##   timbre_4_max      timbre_5_min      timbre_5_max     timbre_6_min     
##  Min.   : -0.651   Min.   :-255.79   Min.   :-22.41   Min.   :-152.170  
##  1st Qu.: 83.563   1st Qu.:-113.47   1st Qu.: 84.50   1st Qu.: -94.861  
##  Median :107.287   Median : -95.46   Median :119.89   Median : -80.382  
##  Mean   :108.045   Mean   :-103.82   Mean   :126.84   Mean   : -80.889  
##  3rd Qu.:130.261   3rd Qu.: -81.01   3rd Qu.:162.12   3rd Qu.: -66.293  
##  Max.   :257.801   Max.   : -42.17   Max.   :349.38   Max.   :   4.503  
##   timbre_6_max     timbre_7_min       timbre_7_max     timbre_8_min     
##  Min.   : 12.70   Min.   :-214.791   Min.   : 15.70   Min.   :-158.756  
##  1st Qu.: 58.79   1st Qu.:-101.146   1st Qu.: 76.38   1st Qu.: -73.143  
##  Median : 70.23   Median : -81.484   Median : 94.47   Median : -62.611  
##  Mean   : 72.00   Mean   : -84.227   Mean   : 95.57   Mean   : -63.686  
##  3rd Qu.: 83.07   3rd Qu.: -64.107   3rd Qu.:112.79   3rd Qu.: -52.921  
##  Max.   :208.39   Max.   :   5.153   Max.   :214.82   Max.   :  -2.382  
##   timbre_8_max     timbre_9_min      timbre_9_max     timbre_10_min    
##  Min.   :-25.95   Min.   :-149.51   Min.   :  8.415   Min.   :-208.82  
##  1st Qu.: 40.58   1st Qu.: -70.26   1st Qu.: 52.815   1st Qu.:-105.15  
##  Median : 49.24   Median : -58.62   Median : 65.716   Median : -83.00  
##  Mean   : 50.08   Mean   : -59.47   Mean   : 67.879   Mean   : -87.26  
##  3rd Qu.: 58.51   3rd Qu.: -47.65   3rd Qu.: 81.144   3rd Qu.: -64.28  
##  Max.   :144.99   Max.   :   1.14   Max.   :161.518   Max.   : -10.64  
##  timbre_10_max     timbre_11_min      timbre_11_max        Top10       
##  Min.   : -6.359   Min.   :-145.599   Min.   :  7.20   Min.   :0.0000  
##  1st Qu.: 39.211   1st Qu.: -58.111   1st Qu.: 39.03   1st Qu.:0.0000  
##  Median : 50.955   Median : -50.932   Median : 46.43   Median :0.0000  
##  Mean   : 55.573   Mean   : -50.869   Mean   : 47.55   Mean   :0.1472  
##  3rd Qu.: 66.654   3rd Qu.: -43.285   3rd Qu.: 55.18   3rd Qu.:0.0000  
##  Max.   :182.724   Max.   :  -6.497   Max.   :110.27   Max.   :1.0000  
##      .rnorm      
##  Min.   :0.6301  
##  1st Qu.:0.6301  
##  Median :0.6301  
##  Mean   :0.6301  
##  3rd Qu.:0.6301  
##  Max.   :0.6301  
##                     year                songtitle               artistname 
##                        0                        0                        0 
##                   songID                 artistID            timesignature 
##                        0                        0                        0 
## timesignature_confidence                 loudness                    tempo 
##                        0                        0                        0 
##         tempo_confidence                      key           key_confidence 
##                        0                        0                        0 
##                   energy                    pitch             timbre_0_min 
##                        0                        0                        0 
##             timbre_0_max             timbre_1_min             timbre_1_max 
##                        0                        0                        0 
##             timbre_2_min             timbre_2_max             timbre_3_min 
##                        0                        0                        0 
##             timbre_3_max             timbre_4_min             timbre_4_max 
##                        0                        0                        0 
##             timbre_5_min             timbre_5_max             timbre_6_min 
##                        0                        0                        0 
##             timbre_6_max             timbre_7_min             timbre_7_max 
##                        0                        0                        0 
##             timbre_8_min             timbre_8_max             timbre_9_min 
##                        0                        0                        0 
##             timbre_9_max            timbre_10_min            timbre_10_max 
##                        0                        0                        0 
##            timbre_11_min            timbre_11_max                    Top10 
##                        0                        0                        0 
##                   .rnorm 
##                        0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##       year       songtitle          artistname           songID         
##  Min.   :2010   Length:373         Length:373         Length:373        
##  1st Qu.:2010   Class :character   Class :character   Class :character  
##  Median :2010   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2010                                                           
##  3rd Qu.:2010                                                           
##  Max.   :2010                                                           
##    artistID         timesignature   timesignature_confidence
##  Length:373         Min.   :1.000   Min.   :0.0000          
##  Class :character   1st Qu.:4.000   1st Qu.:0.8040          
##  Mode  :character   Median :4.000   Median :0.9670          
##                     Mean   :3.895   Mean   :0.8528          
##                     3rd Qu.:4.000   3rd Qu.:1.0000          
##                     Max.   :7.000   Max.   :1.0000          
##     loudness           tempo        tempo_confidence      key       
##  Min.   :-22.306   Min.   : 66.66   Min.   :0.000    Min.   : 0.00  
##  1st Qu.: -8.882   1st Qu.: 87.06   1st Qu.:0.347    1st Qu.: 2.00  
##  Median : -6.370   Median :108.05   Median :0.610    Median : 6.00  
##  Mean   : -7.478   Mean   :109.82   Mean   :0.586    Mean   : 5.45  
##  3rd Qu.: -4.780   3rd Qu.:128.01   3rd Qu.:0.857    3rd Qu.: 9.00  
##  Max.   : -0.576   Max.   :204.92   Max.   :1.000    Max.   :11.00  
##  key_confidence       energy           pitch          timbre_0_min   
##  Min.   :0.0000   Min.   :0.0106   Min.   :0.00000   Min.   : 0.000  
##  1st Qu.:0.1720   1st Qu.:0.5772   1st Qu.:0.00400   1st Qu.: 0.000  
##  Median :0.4420   Median :0.7657   Median :0.00700   Median : 0.029  
##  Mean   :0.4091   Mean   :0.7148   Mean   :0.01029   Mean   : 5.480  
##  3rd Qu.:0.6270   3rd Qu.:0.9092   3rd Qu.:0.01300   3rd Qu.: 7.042  
##  Max.   :1.0000   Max.   :0.9974   Max.   :0.09500   Max.   :43.044  
##   timbre_0_max    timbre_1_min      timbre_1_max     timbre_2_min     
##  Min.   :41.94   Min.   :-325.73   Min.   : 97.82   Min.   :-272.039  
##  1st Qu.:54.48   1st Qu.:-167.91   1st Qu.:171.13   1st Qu.:-160.611  
##  Median :56.45   Median :-108.85   Median :194.43   Median :-132.648  
##  Mean   :55.53   Mean   :-117.33   Mean   :212.70   Mean   :-131.282  
##  3rd Qu.:57.55   3rd Qu.: -64.10   3rd Qu.:236.97   3rd Qu.:-101.806  
##  Max.   :60.53   Max.   :  51.34   Max.   :485.30   Max.   :   3.717  
##   timbre_2_max     timbre_3_min      timbre_3_max     timbre_4_min     
##  Min.   : 47.15   Min.   :-473.15   Min.   : 41.43   Min.   :-122.212  
##  1st Qu.:115.09   1st Qu.:-233.35   1st Qu.:128.71   1st Qu.: -72.458  
##  Median :137.86   Median :-171.55   Median :190.07   Median : -60.243  
##  Mean   :141.94   Mean   :-190.39   Mean   :216.83   Mean   : -61.109  
##  3rd Qu.:168.57   3rd Qu.:-129.32   3rd Qu.:315.92   3rd Qu.: -49.108  
##  Max.   :287.96   Max.   : -42.49   Max.   :449.45   Max.   :   4.647  
##   timbre_4_max     timbre_5_min      timbre_5_max      timbre_6_min    
##  Min.   : 49.63   Min.   :-262.48   Min.   :  6.061   Min.   :-135.82  
##  1st Qu.: 89.94   1st Qu.:-115.86   1st Qu.: 88.959   1st Qu.: -94.15  
##  Median :111.48   Median : -95.47   Median :119.965   Median : -80.97  
##  Mean   :111.74   Mean   :-107.51   Mean   :130.958   Mean   : -82.01  
##  3rd Qu.:131.49   3rd Qu.: -81.75   3rd Qu.:166.842   3rd Qu.: -68.74  
##  Max.   :199.50   Max.   : -57.76   Max.   :350.936   Max.   : -24.34  
##   timbre_6_max     timbre_7_min      timbre_7_max     timbre_8_min    
##  Min.   : 18.93   Min.   :-169.28   Min.   : 42.19   Min.   :-118.97  
##  1st Qu.: 64.05   1st Qu.:-101.35   1st Qu.: 78.18   1st Qu.: -71.64  
##  Median : 73.57   Median : -86.67   Median : 97.94   Median : -64.01  
##  Mean   : 75.29   Mean   : -85.96   Mean   : 97.28   Mean   : -64.04  
##  3rd Qu.: 87.61   3rd Qu.: -68.55   3rd Qu.:111.17   3rd Qu.: -54.20  
##  Max.   :140.77   Max.   : -29.69   Max.   :196.31   Max.   : -18.94  
##   timbre_8_max    timbre_9_min      timbre_9_max    timbre_10_min    
##  Min.   :19.55   Min.   :-122.29   Min.   : 14.19   Min.   :-202.26  
##  1st Qu.:40.77   1st Qu.: -70.80   1st Qu.: 56.76   1st Qu.:-104.81  
##  Median :48.93   Median : -59.01   Median : 69.53   Median : -84.41  
##  Mean   :49.61   Mean   : -60.32   Mean   : 70.91   Mean   : -88.80  
##  3rd Qu.:57.55   3rd Qu.: -48.07   3rd Qu.: 82.99   3rd Qu.: -67.31  
##  Max.   :96.35   Max.   : -16.73   Max.   :156.53   Max.   : -30.34  
##  timbre_10_max    timbre_11_min    timbre_11_max       Top10       
##  Min.   : 14.17   Min.   :-95.05   Min.   :21.70   Min.   :0.0000  
##  1st Qu.: 38.85   1st Qu.:-57.22   1st Qu.:38.08   1st Qu.:0.0000  
##  Median : 49.93   Median :-50.41   Median :46.47   Median :0.0000  
##  Mean   : 54.51   Mean   :-50.86   Mean   :46.44   Mean   :0.1582  
##  3rd Qu.: 65.82   3rd Qu.:-43.56   3rd Qu.:53.44   3rd Qu.:0.0000  
##  Max.   :192.42   Max.   :-21.55   Max.   :87.92   Max.   :1.0000  
##      .rnorm       
##  Min.   :-0.2762  
##  1st Qu.:-0.2762  
##  Median :-0.2762  
##  Mean   :-0.2762  
##  3rd Qu.:-0.2762  
##  Max.   :-0.2762  
##                     year                songtitle               artistname 
##                        0                        0                        0 
##                   songID                 artistID            timesignature 
##                        0                        0                        0 
## timesignature_confidence                 loudness                    tempo 
##                        0                        0                        0 
##         tempo_confidence                      key           key_confidence 
##                        0                        0                        0 
##                   energy                    pitch             timbre_0_min 
##                        0                        0                        0 
##             timbre_0_max             timbre_1_min             timbre_1_max 
##                        0                        0                        0 
##             timbre_2_min             timbre_2_max             timbre_3_min 
##                        0                        0                        0 
##             timbre_3_max             timbre_4_min             timbre_4_max 
##                        0                        0                        0 
##             timbre_5_min             timbre_5_max             timbre_6_min 
##                        0                        0                        0 
##             timbre_6_max             timbre_7_min             timbre_7_max 
##                        0                        0                        0 
##             timbre_8_min             timbre_8_max             timbre_9_min 
##                        0                        0                        0 
##             timbre_9_max            timbre_10_min            timbre_10_max 
##                        0                        0                        0 
##            timbre_11_min            timbre_11_max                    Top10 
##                        0                        0                        0 
##                   .rnorm 
##                        0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_newent_df
# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(table(sign(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](songs_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_predct_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
        
        # Partition again
        glb_entity_df <<- subset(union_df, .src == "entity", select=-.src)
        comment(glb_entity_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-.src)
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if ((sum(sapply(names(glb_entity_df), 
                function(col) sum(is.na(glb_entity_df[, col])))) > 0) | 
    (sum(sapply(names(glb_newent_df), 
                function(col) sum(is.na(glb_newent_df[, col])))) > 0))
    glb_impute_missing_data(glb_entity_df, glb_newent_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##                                                id        cor.y   cor.y.abs
## timbre_6_min                         timbre_6_min -0.216378268 0.216378268
## timbre_4_max                         timbre_4_max  0.197034666 0.197034666
## timbre_11_min                       timbre_11_min -0.193049686 0.193049686
## timbre_8_max                         timbre_8_max  0.162722687 0.162722687
## timbre_7_min                         timbre_7_min -0.155645339 0.155645339
## timbre_0_max                         timbre_0_max -0.152171355 0.152171355
## timbre_9_max                         timbre_9_max  0.145756048 0.145756048
## timbre_11_max                       timbre_11_max  0.143718363 0.143718363
## timbre_2_min                         timbre_2_min -0.136085935 0.136085935
## pitch                                       pitch -0.135723510 0.135723510
## timbre_5_min                         timbre_5_min -0.123590779 0.123590779
## timbre_6_max                         timbre_6_max  0.123432076 0.123432076
## energy                                     energy -0.120897670 0.120897670
## timbre_1_max                         timbre_1_max  0.106338252 0.106338252
## timbre_10_max                       timbre_10_max  0.104441775 0.104441775
## timbre_8_min                         timbre_8_min -0.101131084 0.101131084
## timbre_2_max                         timbre_2_max  0.096545551 0.096545551
## timbre_9_min                         timbre_9_min -0.088167503 0.088167503
## loudness                                 loudness -0.086546031 0.086546031
## tempo_confidence                 tempo_confidence  0.086315020 0.086315020
## timbre_5_max                         timbre_5_max  0.076923330 0.076923330
## timbre_0_min                         timbre_0_min  0.068825164 0.068825164
## timesignature_confidence timesignature_confidence  0.063023674 0.063023674
## timesignature                       timesignature  0.046630910 0.046630910
## timbre_7_max                         timbre_7_max  0.045966630 0.045966630
## timbre_3_max                         timbre_3_max -0.026744876 0.026744876
## timbre_3_min                         timbre_3_min -0.026442066 0.026442066
## key                                           key  0.025326063 0.025326063
## key_confidence                     key_confidence  0.010021403 0.010021403
## timbre_4_min                         timbre_4_min -0.006010064 0.006010064
## timbre_10_min                       timbre_10_min  0.005257590 0.005257590
## timbre_1_min                         timbre_1_min  0.003659089 0.003659089
## tempo                                       tempo -0.003577709 0.003577709
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                          timbre_6_min timbre_4_max timbre_11_min
## timbre_6_min               1.00000000 -0.455025649   0.273390352
## timbre_4_max              -0.45502565  1.000000000  -0.302295478
## timbre_11_min              0.27339035 -0.302295478   1.000000000
## timbre_8_max              -0.34336035  0.295179151  -0.272093182
## timbre_7_min               0.45337904 -0.416885588   0.243882172
## timbre_0_max               0.06960243 -0.013662234  -0.027117957
## timbre_9_max              -0.35039880  0.299157415  -0.197884104
## timbre_11_max             -0.28762766  0.269329755  -0.100340776
## timbre_2_min               0.33468254 -0.280997994   0.212846357
## pitch                      0.27804672 -0.302106679   0.261538974
## timbre_5_min               0.23066690 -0.244348086   0.205150079
## timbre_6_max              -0.10989206  0.299433795  -0.283300815
## energy                     0.29475723 -0.289538100   0.066856448
## timbre_1_max              -0.30491475  0.316226992  -0.165638653
## timbre_10_max             -0.17479426  0.104252143  -0.105775522
## timbre_8_min               0.28075443 -0.391282450   0.288524969
## timbre_2_max              -0.23024146  0.395687715  -0.253280754
## timbre_9_min               0.35086071 -0.377463693   0.189779797
## loudness                   0.12085527 -0.093376576  -0.037403437
## tempo_confidence          -0.01786080  0.040958171  -0.093264502
## timbre_5_max              -0.20040441  0.109963011  -0.126617896
## timbre_0_min               0.04825382 -0.044692613   0.011006016
## timesignature_confidence  -0.03136163  0.080582500  -0.073911622
## timesignature             -0.02908114  0.038111591  -0.071138001
## timbre_7_max              -0.29687285  0.243897425  -0.121150310
## timbre_3_max              -0.15303825 -0.009126047  -0.020697075
## timbre_3_min               0.18209077 -0.110443035   0.063311013
## key                       -0.01755538  0.005219950   0.007080471
## key_confidence            -0.01894425  0.027472060  -0.033262222
## timbre_4_min               0.07040792  0.136746164   0.210352846
## timbre_10_min              0.16163316 -0.043954487   0.064520244
## timbre_1_min               0.20827637 -0.352448987   0.135958877
## tempo                      0.07772488 -0.084994775   0.003122385
##                          timbre_8_max timbre_7_min  timbre_0_max
## timbre_6_min             -0.343360348  0.453379045  0.0696024349
## timbre_4_max              0.295179151 -0.416885588 -0.0136622340
## timbre_11_min            -0.272093182  0.243882172 -0.0271179570
## timbre_8_max              1.000000000 -0.311466153 -0.1395411785
## timbre_7_min             -0.311466153  1.000000000  0.0378223542
## timbre_0_max             -0.139541178  0.037822354  1.0000000000
## timbre_9_max              0.273580091 -0.480306074 -0.0481018049
## timbre_11_max             0.317062203 -0.212158521 -0.1192869292
## timbre_2_min             -0.262841319  0.382043213  0.0402265624
## pitch                    -0.294399713  0.336376154 -0.0493222539
## timbre_5_min             -0.195179188  0.264235231 -0.0470870381
## timbre_6_max              0.220280830 -0.230236051  0.0059826955
## energy                   -0.282351251  0.329769434  0.5632351479
## timbre_1_max              0.147887015 -0.319382009  0.0462136599
## timbre_10_max             0.108968439 -0.172849789  0.0459537656
## timbre_8_min             -0.102824593  0.337189672  0.0240473720
## timbre_2_max              0.154130354 -0.147575396  0.0585797233
## timbre_9_min             -0.247454393  0.480227550  0.0327041012
## loudness                 -0.166991833  0.095301520  0.9058955144
## tempo_confidence         -0.004713273 -0.051643291  0.0774819401
## timbre_5_max              0.096891888 -0.224828057  0.0268005031
## timbre_0_min              0.001905078  0.042752032 -0.0474829253
## timesignature_confidence -0.034307220 -0.073592966  0.1676558113
## timesignature             0.014355589 -0.048818652  0.1000202527
## timbre_7_max              0.157315943 -0.396808582  0.0131923760
## timbre_3_max             -0.016934165 -0.077538391  0.1384427330
## timbre_3_min             -0.114592983  0.312779539 -0.0475486231
## key                       0.003711362  0.003365663 -0.0154776480
## key_confidence            0.003680199 -0.022219296  0.0606462420
## timbre_4_min             -0.154246437  0.080236736  0.0429582944
## timbre_10_min            -0.009578378  0.188496107 -0.1625037970
## timbre_1_min             -0.286368842  0.375513659  0.0982142401
## tempo                    -0.018156497  0.089338029  0.0006267798
##                          timbre_9_max timbre_11_max timbre_2_min
## timbre_6_min             -0.350398795  -0.287627661   0.33468254
## timbre_4_max              0.299157415   0.269329755  -0.28099799
## timbre_11_min            -0.197884104  -0.100340776   0.21284636
## timbre_8_max              0.273580091   0.317062203  -0.26284132
## timbre_7_min             -0.480306074  -0.212158521   0.38204321
## timbre_0_max             -0.048101805  -0.119286929   0.04022656
## timbre_9_max              1.000000000   0.148938415  -0.33962450
## timbre_11_max             0.148938415   1.000000000  -0.18760710
## timbre_2_min             -0.339624497  -0.187607098   1.00000000
## pitch                    -0.218041638  -0.200903150   0.35066686
## timbre_5_min             -0.243147818  -0.124021662   0.42052948
## timbre_6_max              0.233402549   0.293563661  -0.24058235
## energy                   -0.152416686  -0.243295160   0.24300973
## timbre_1_max              0.349430332   0.176178429  -0.24591835
## timbre_10_max             0.280516892   0.109054150  -0.22991416
## timbre_8_min             -0.229929550  -0.301381265   0.25895382
## timbre_2_max              0.134203698   0.204646489   0.06892429
## timbre_9_min             -0.311198690  -0.232306316   0.44355187
## loudness                 -0.026380233  -0.173335027   0.07864783
## tempo_confidence          0.080879806  -0.039039039  -0.13353078
## timbre_5_max              0.393712297   0.064908698  -0.24964052
## timbre_0_min              0.010839078  -0.061334471   0.13387939
## timesignature_confidence  0.068768307  -0.031432484  -0.11076993
## timesignature             0.035693641  -0.002586617  -0.03971574
## timbre_7_max              0.367655592   0.123413462  -0.32996685
## timbre_3_max              0.151310307  -0.002925378  -0.29105273
## timbre_3_min             -0.326593257  -0.091720171   0.21467925
## key                       0.008604285   0.017008236  -0.03457335
## key_confidence           -0.024188984   0.032322936   0.01670375
## timbre_4_min             -0.123143414  -0.131563583   0.17753915
## timbre_10_min            -0.259533956   0.009378114   0.28249002
## timbre_1_min             -0.187687648  -0.203375969   0.32539515
## tempo                    -0.049463498  -0.012129520   0.04339832
##                                pitch timbre_5_min timbre_6_max
## timbre_6_min              0.27804672  0.230666903 -0.109892063
## timbre_4_max             -0.30210668 -0.244348086  0.299433795
## timbre_11_min             0.26153897  0.205150079 -0.283300815
## timbre_8_max             -0.29439971 -0.195179188  0.220280830
## timbre_7_min              0.33637615  0.264235231 -0.230236051
## timbre_0_max             -0.04932225 -0.047087038  0.005982696
## timbre_9_max             -0.21804164 -0.243147818  0.233402549
## timbre_11_max            -0.20090315 -0.124021662  0.293563661
## timbre_2_min              0.35066686  0.420529476 -0.240582350
## pitch                     1.00000000  0.223960194 -0.219001377
## timbre_5_min              0.22396019  1.000000000 -0.226504870
## timbre_6_max             -0.21900138 -0.226504870  1.000000000
## energy                    0.31226266  0.036369274 -0.098479283
## timbre_1_max             -0.09810809 -0.191875077  0.166754561
## timbre_10_max            -0.08725314 -0.184676809  0.089273384
## timbre_8_min              0.22939066  0.218309873 -0.356422030
## timbre_2_max             -0.06848500 -0.089580677  0.367687159
## timbre_9_min              0.28790551  0.416216275 -0.248575771
## loudness                  0.05656993 -0.059089992 -0.005232245
## tempo_confidence         -0.05232348 -0.108568422  0.062042883
## timbre_5_max             -0.10359583 -0.144038997  0.156548395
## timbre_0_min              0.05818190  0.030193786  0.006021398
## timesignature_confidence -0.09687713 -0.111815829  0.076600503
## timesignature            -0.09444193 -0.051730222  0.055446717
## timbre_7_max             -0.18438616 -0.259243709  0.195604792
## timbre_3_max              0.03334388 -0.406337856  0.082302782
## timbre_3_min              0.13616712  0.092879786 -0.186143204
## key                       0.01145964 -0.023763318  0.032824721
## key_confidence           -0.08485858  0.021182571 -0.004471869
## timbre_4_min              0.10820600  0.138930408 -0.181223606
## timbre_10_min             0.01886741  0.125766699 -0.094548646
## timbre_1_min              0.39195512  0.185315298 -0.200257312
## tempo                     0.03859802  0.008690528 -0.017609126
##                                energy timbre_1_max timbre_10_max
## timbre_6_min              0.294757232  -0.30491475  -0.174794261
## timbre_4_max             -0.289538100   0.31622699   0.104252143
## timbre_11_min             0.066856448  -0.16563865  -0.105775522
## timbre_8_max             -0.282351251   0.14788701   0.108968439
## timbre_7_min              0.329769434  -0.31938201  -0.172849789
## timbre_0_max              0.563235148   0.04621366   0.045953766
## timbre_9_max             -0.152416686   0.34943033   0.280516892
## timbre_11_max            -0.243295160   0.17617843   0.109054150
## timbre_2_min              0.243009734  -0.24591835  -0.229914156
## pitch                     0.312262657  -0.09810809  -0.087253145
## timbre_5_min              0.036369274  -0.19187508  -0.184676809
## timbre_6_max             -0.098479283   0.16675456   0.089273384
## energy                    1.000000000   0.00200121   0.022486279
## timbre_1_max              0.002001210   1.00000000   0.145669916
## timbre_10_max             0.022486279   0.14566992   1.000000000
## timbre_8_min              0.182794170  -0.31173316  -0.089497147
## timbre_2_max              0.013518960   0.28261858   0.076449437
## timbre_9_min              0.317943097  -0.20999204  -0.168599300
## loudness                  0.739906708   0.04843218   0.092053468
## tempo_confidence          0.151835896   0.04626091   0.082235706
## timbre_5_max             -0.041564526   0.15094184   0.342166572
## timbre_0_min              0.090120314  -0.08527484  -0.044939690
## timesignature_confidence  0.105931843   0.04757327   0.024220307
## timesignature             0.086373400   0.02304274   0.024450016
## timbre_7_max             -0.172629495   0.22382410   0.190467297
## timbre_3_max              0.160216580   0.14297015   0.179598318
## timbre_3_min              0.153648190  -0.09181731  -0.071899263
## key                       0.006268243   0.00835810   0.012914541
## key_confidence           -0.055377445  -0.04455801  -0.014574252
## timbre_4_min             -0.039110291  -0.15063941  -0.105485194
## timbre_10_min            -0.108898889  -0.20307467  -0.189783690
## timbre_1_min              0.484662808   0.05414452  -0.020543933
## tempo                     0.154989603  -0.04376397  -0.007268116
##                          timbre_8_min  timbre_2_max timbre_9_min
## timbre_6_min              0.280754431 -0.2302414573   0.35086071
## timbre_4_max             -0.391282450  0.3956877151  -0.37746369
## timbre_11_min             0.288524969 -0.2532807536   0.18977980
## timbre_8_max             -0.102824593  0.1541303543  -0.24745439
## timbre_7_min              0.337189672 -0.1475753958   0.48022755
## timbre_0_max              0.024047372  0.0585797233   0.03270410
## timbre_9_max             -0.229929550  0.1342036983  -0.31119869
## timbre_11_max            -0.301381265  0.2046464888  -0.23230632
## timbre_2_min              0.258953817  0.0689242891   0.44355187
## pitch                     0.229390664 -0.0684850040   0.28790551
## timbre_5_min              0.218309873 -0.0895806765   0.41621628
## timbre_6_max             -0.356422030  0.3676871592  -0.24857577
## energy                    0.182794170  0.0135189596   0.31794310
## timbre_1_max             -0.311733156  0.2826185842  -0.20999204
## timbre_10_max            -0.089497147  0.0764494369  -0.16859930
## timbre_8_min              1.000000000 -0.2790849287   0.35423560
## timbre_2_max             -0.279084929  1.0000000000  -0.11535359
## timbre_9_min              0.354235603 -0.1153535855   1.00000000
## loudness                  0.072235220  0.0528818381   0.09981948
## tempo_confidence         -0.042807148 -0.0327553005  -0.03285915
## timbre_5_max             -0.087237336  0.0379334991  -0.19928533
## timbre_0_min              0.060985110  0.1071360022   0.10070341
## timesignature_confidence -0.079158394 -0.0018110808  -0.05111407
## timesignature            -0.050334757  0.0197393819  -0.01465327
## timbre_7_max             -0.229674385  0.0948650045  -0.43881220
## timbre_3_max             -0.045053702  0.0698456281  -0.26829132
## timbre_3_min              0.104275059 -0.0733918083   0.18491055
## key                      -0.019667315  0.0182015842  -0.02025505
## key_confidence           -0.005621326  0.0005591351  -0.01834614
## timbre_4_min              0.231026499 -0.0888438468   0.10583691
## timbre_10_min             0.080554935 -0.0691323664   0.13245828
## timbre_1_min              0.219176282 -0.0355838445   0.44636549
## tempo                     0.071572973 -0.0136902058   0.07339777
##                              loudness tempo_confidence timbre_5_max
## timbre_6_min              0.120855271     -0.017860800 -0.200404407
## timbre_4_max             -0.093376576      0.040958171  0.109963011
## timbre_11_min            -0.037403437     -0.093264502 -0.126617896
## timbre_8_max             -0.166991833     -0.004713273  0.096891888
## timbre_7_min              0.095301520     -0.051643291 -0.224828057
## timbre_0_max              0.905895514      0.077481940  0.026800503
## timbre_9_max             -0.026380233      0.080879806  0.393712297
## timbre_11_max            -0.173335027     -0.039039039  0.064908698
## timbre_2_min              0.078647829     -0.133530783 -0.249640519
## pitch                     0.056569934     -0.052323478 -0.103595831
## timbre_5_min             -0.059089992     -0.108568422 -0.144038997
## timbre_6_max             -0.005232245      0.062042883  0.156548395
## energy                    0.739906708      0.151835896 -0.041564526
## timbre_1_max              0.048432176      0.046260907  0.150941837
## timbre_10_max             0.092053468      0.082235706  0.342166572
## timbre_8_min              0.072235220     -0.042807148 -0.087237336
## timbre_2_max              0.052881838     -0.032755301  0.037933499
## timbre_9_min              0.099819479     -0.032859147 -0.199285332
## loudness                  1.000000000      0.142882827  0.068169858
## tempo_confidence          0.142882827      1.000000000  0.086076636
## timbre_5_max              0.068169858      0.086076636  1.000000000
## timbre_0_min              0.009399084     -0.031845980 -0.024819207
## timesignature_confidence  0.165697849      0.328598944  0.050928400
## timesignature             0.105771537      0.163705257  0.031055225
## timbre_7_max             -0.008656228      0.035375496  0.214537373
## timbre_3_max              0.207245886      0.032902517  0.194677953
## timbre_3_min             -0.016158484      0.013770331 -0.283879738
## key                      -0.007781500      0.013342528  0.008153236
## key_confidence            0.019482412     -0.013629201 -0.020663866
## timbre_4_min             -0.003363140      0.002233332 -0.118015402
## timbre_10_min            -0.211982830     -0.043191735 -0.301989938
## timbre_1_min              0.260369254      0.049810469 -0.049218571
## tempo                     0.048535038     -0.023280660 -0.009726892
##                          timbre_0_min timesignature_confidence
## timbre_6_min              0.048253824             -0.031361625
## timbre_4_max             -0.044692613              0.080582500
## timbre_11_min             0.011006016             -0.073911622
## timbre_8_max              0.001905078             -0.034307220
## timbre_7_min              0.042752032             -0.073592966
## timbre_0_max             -0.047482925              0.167655811
## timbre_9_max              0.010839078              0.068768307
## timbre_11_max            -0.061334471             -0.031432484
## timbre_2_min              0.133879387             -0.110769925
## pitch                     0.058181899             -0.096877126
## timbre_5_min              0.030193786             -0.111815829
## timbre_6_max              0.006021398              0.076600503
## energy                    0.090120314              0.105931843
## timbre_1_max             -0.085274839              0.047573274
## timbre_10_max            -0.044939690              0.024220307
## timbre_8_min              0.060985110             -0.079158394
## timbre_2_max              0.107136002             -0.001811081
## timbre_9_min              0.100703414             -0.051114075
## loudness                  0.009399084              0.165697849
## tempo_confidence         -0.031845980              0.328598944
## timbre_5_max             -0.024819207              0.050928400
## timbre_0_min              1.000000000             -0.026330389
## timesignature_confidence -0.026330389              1.000000000
## timesignature             0.013387463              0.343982155
## timbre_7_max             -0.094520663              0.033254180
## timbre_3_max             -0.082459551              0.004536818
## timbre_3_min             -0.022874423             -0.022642567
## key                      -0.008790121              0.006794214
## key_confidence           -0.044227848             -0.016430285
## timbre_4_min              0.020810701              0.020021944
## timbre_10_min             0.041833097             -0.047834679
## timbre_1_min              0.107282699             -0.024430815
## tempo                     0.021308733             -0.029783675
##                          timesignature timbre_7_max timbre_3_max
## timbre_6_min              -0.029081140 -0.296872849 -0.153038250
## timbre_4_max               0.038111591  0.243897425 -0.009126047
## timbre_11_min             -0.071138001 -0.121150310 -0.020697075
## timbre_8_max               0.014355589  0.157315943 -0.016934165
## timbre_7_min              -0.048818652 -0.396808582 -0.077538391
## timbre_0_max               0.100020253  0.013192376  0.138442733
## timbre_9_max               0.035693641  0.367655592  0.151310307
## timbre_11_max             -0.002586617  0.123413462 -0.002925378
## timbre_2_min              -0.039715742 -0.329966846 -0.291052730
## pitch                     -0.094441934 -0.184386155  0.033343878
## timbre_5_min              -0.051730222 -0.259243709 -0.406337856
## timbre_6_max               0.055446717  0.195604792  0.082302782
## energy                     0.086373400 -0.172629495  0.160216580
## timbre_1_max               0.023042736  0.223824104  0.142970150
## timbre_10_max              0.024450016  0.190467297  0.179598318
## timbre_8_min              -0.050334757 -0.229674385 -0.045053702
## timbre_2_max               0.019739382  0.094865005  0.069845628
## timbre_9_min              -0.014653267 -0.438812202 -0.268291318
## loudness                   0.105771537 -0.008656228  0.207245886
## tempo_confidence           0.163705257  0.035375496  0.032902517
## timbre_5_max               0.031055225  0.214537373  0.194677953
## timbre_0_min               0.013387463 -0.094520663 -0.082459551
## timesignature_confidence   0.343982155  0.033254180  0.004536818
## timesignature              1.000000000  0.002040133 -0.022124539
## timbre_7_max               0.002040133  1.000000000  0.340934695
## timbre_3_max              -0.022124539  0.340934695  1.000000000
## timbre_3_min               0.006046027 -0.200025491 -0.132306323
## key                        0.017572596  0.005510934  0.010024050
## key_confidence            -0.015918168 -0.003676716 -0.049084134
## timbre_4_min              -0.004021022 -0.084025763 -0.106272159
## timbre_10_min             -0.008729925 -0.248894049 -0.509846782
## timbre_1_min              -0.016925000 -0.275511097  0.109976920
## tempo                      0.048710910 -0.075725817 -0.005910305
##                           timbre_3_min          key key_confidence
## timbre_6_min              0.1820907729 -0.017555376  -0.0189442544
## timbre_4_max             -0.1104430347  0.005219950   0.0274720602
## timbre_11_min             0.0633110132  0.007080471  -0.0332622220
## timbre_8_max             -0.1145929826  0.003711362   0.0036801985
## timbre_7_min              0.3127795392  0.003365663  -0.0222192962
## timbre_0_max             -0.0475486231 -0.015477648   0.0606462420
## timbre_9_max             -0.3265932568  0.008604285  -0.0241889840
## timbre_11_max            -0.0917201714  0.017008236   0.0323229361
## timbre_2_min              0.2146792494 -0.034573353   0.0167037493
## pitch                     0.1361671151  0.011459639  -0.0848585775
## timbre_5_min              0.0928797862 -0.023763318   0.0211825706
## timbre_6_max             -0.1861432039  0.032824721  -0.0044718691
## energy                    0.1536481903  0.006268243  -0.0553774453
## timbre_1_max             -0.0918173123  0.008358100  -0.0445580080
## timbre_10_max            -0.0718992627  0.012914541  -0.0145742522
## timbre_8_min              0.1042750586 -0.019667315  -0.0056213263
## timbre_2_max             -0.0733918083  0.018201584   0.0005591351
## timbre_9_min              0.1849105474 -0.020255052  -0.0183461407
## loudness                 -0.0161584843 -0.007781500   0.0194824125
## tempo_confidence          0.0137703313  0.013342528  -0.0136292011
## timbre_5_max             -0.2838797383  0.008153236  -0.0206638661
## timbre_0_min             -0.0228744234 -0.008790121  -0.0442278483
## timesignature_confidence -0.0226425671  0.006794214  -0.0164302851
## timesignature             0.0060460266  0.017572596  -0.0159181676
## timbre_7_max             -0.2000254906  0.005510934  -0.0036767165
## timbre_3_max             -0.1323063227  0.010024050  -0.0490841335
## timbre_3_min              1.0000000000 -0.008042766  -0.0003020344
## key                      -0.0080427657  1.000000000  -0.0435718053
## key_confidence           -0.0003020344 -0.043571805   1.0000000000
## timbre_4_min              0.0599235058 -0.027567496   0.0249461462
## timbre_10_min             0.2514500287  0.007024254   0.0604936404
## timbre_1_min              0.1324010792  0.010649185  -0.0757591439
## tempo                     0.0479623596  0.018874310   0.0709959251
##                          timbre_4_min timbre_10_min timbre_1_min
## timbre_6_min              0.070407925   0.161633164   0.20827637
## timbre_4_max              0.136746164  -0.043954487  -0.35244899
## timbre_11_min             0.210352846   0.064520244   0.13595888
## timbre_8_max             -0.154246437  -0.009578378  -0.28636884
## timbre_7_min              0.080236736   0.188496107   0.37551366
## timbre_0_max              0.042958294  -0.162503797   0.09821424
## timbre_9_max             -0.123143414  -0.259533956  -0.18768765
## timbre_11_max            -0.131563583   0.009378114  -0.20337597
## timbre_2_min              0.177539148   0.282490023   0.32539515
## pitch                     0.108205998   0.018867411   0.39195512
## timbre_5_min              0.138930408   0.125766699   0.18531530
## timbre_6_max             -0.181223606  -0.094548646  -0.20025731
## energy                   -0.039110291  -0.108898889   0.48466281
## timbre_1_max             -0.150639406  -0.203074667   0.05414452
## timbre_10_max            -0.105485194  -0.189783690  -0.02054393
## timbre_8_min              0.231026499   0.080554935   0.21917628
## timbre_2_max             -0.088843847  -0.069132366  -0.03558384
## timbre_9_min              0.105836913   0.132458282   0.44636549
## loudness                 -0.003363140  -0.211982830   0.26036925
## tempo_confidence          0.002233332  -0.043191735   0.04981047
## timbre_5_max             -0.118015402  -0.301989938  -0.04921857
## timbre_0_min              0.020810701   0.041833097   0.10728270
## timesignature_confidence  0.020021944  -0.047834679  -0.02443081
## timesignature            -0.004021022  -0.008729925  -0.01692500
## timbre_7_max             -0.084025763  -0.248894049  -0.27551110
## timbre_3_max             -0.106272159  -0.509846782   0.10997692
## timbre_3_min              0.059923506   0.251450029   0.13240108
## key                      -0.027567496   0.007024254   0.01064919
## key_confidence            0.024946146   0.060493640  -0.07575914
## timbre_4_min              1.000000000   0.114621536   0.02638199
## timbre_10_min             0.114621536   1.000000000  -0.05979506
## timbre_1_min              0.026381988  -0.059795059   1.00000000
## tempo                    -0.018817596   0.031374782   0.07940409
##                                  tempo
## timbre_6_min              0.0777248783
## timbre_4_max             -0.0849947746
## timbre_11_min             0.0031223853
## timbre_8_max             -0.0181564969
## timbre_7_min              0.0893380293
## timbre_0_max              0.0006267798
## timbre_9_max             -0.0494634981
## timbre_11_max            -0.0121295203
## timbre_2_min              0.0433983220
## pitch                     0.0385980153
## timbre_5_min              0.0086905285
## timbre_6_max             -0.0176091258
## energy                    0.1549896030
## timbre_1_max             -0.0437639708
## timbre_10_max            -0.0072681162
## timbre_8_min              0.0715729733
## timbre_2_max             -0.0136902058
## timbre_9_min              0.0733977686
## loudness                  0.0485350385
## tempo_confidence         -0.0232806600
## timbre_5_max             -0.0097268920
## timbre_0_min              0.0213087331
## timesignature_confidence -0.0297836750
## timesignature             0.0487109099
## timbre_7_max             -0.0757258169
## timbre_3_max             -0.0059103046
## timbre_3_min              0.0479623596
## key                       0.0188743099
## key_confidence            0.0709959251
## timbre_4_min             -0.0188175962
## timbre_10_min             0.0313747819
## timbre_1_min              0.0794040880
## tempo                     1.0000000000
##                          timbre_6_min timbre_4_max timbre_11_min
## timbre_6_min               0.00000000  0.455025649   0.273390352
## timbre_4_max               0.45502565  0.000000000   0.302295478
## timbre_11_min              0.27339035  0.302295478   0.000000000
## timbre_8_max               0.34336035  0.295179151   0.272093182
## timbre_7_min               0.45337904  0.416885588   0.243882172
## timbre_0_max               0.06960243  0.013662234   0.027117957
## timbre_9_max               0.35039880  0.299157415   0.197884104
## timbre_11_max              0.28762766  0.269329755   0.100340776
## timbre_2_min               0.33468254  0.280997994   0.212846357
## pitch                      0.27804672  0.302106679   0.261538974
## timbre_5_min               0.23066690  0.244348086   0.205150079
## timbre_6_max               0.10989206  0.299433795   0.283300815
## energy                     0.29475723  0.289538100   0.066856448
## timbre_1_max               0.30491475  0.316226992   0.165638653
## timbre_10_max              0.17479426  0.104252143   0.105775522
## timbre_8_min               0.28075443  0.391282450   0.288524969
## timbre_2_max               0.23024146  0.395687715   0.253280754
## timbre_9_min               0.35086071  0.377463693   0.189779797
## loudness                   0.12085527  0.093376576   0.037403437
## tempo_confidence           0.01786080  0.040958171   0.093264502
## timbre_5_max               0.20040441  0.109963011   0.126617896
## timbre_0_min               0.04825382  0.044692613   0.011006016
## timesignature_confidence   0.03136163  0.080582500   0.073911622
## timesignature              0.02908114  0.038111591   0.071138001
## timbre_7_max               0.29687285  0.243897425   0.121150310
## timbre_3_max               0.15303825  0.009126047   0.020697075
## timbre_3_min               0.18209077  0.110443035   0.063311013
## key                        0.01755538  0.005219950   0.007080471
## key_confidence             0.01894425  0.027472060   0.033262222
## timbre_4_min               0.07040792  0.136746164   0.210352846
## timbre_10_min              0.16163316  0.043954487   0.064520244
## timbre_1_min               0.20827637  0.352448987   0.135958877
## tempo                      0.07772488  0.084994775   0.003122385
##                          timbre_8_max timbre_7_min timbre_0_max
## timbre_6_min              0.343360348  0.453379045 0.0696024349
## timbre_4_max              0.295179151  0.416885588 0.0136622340
## timbre_11_min             0.272093182  0.243882172 0.0271179570
## timbre_8_max              0.000000000  0.311466153 0.1395411785
## timbre_7_min              0.311466153  0.000000000 0.0378223542
## timbre_0_max              0.139541178  0.037822354 0.0000000000
## timbre_9_max              0.273580091  0.480306074 0.0481018049
## timbre_11_max             0.317062203  0.212158521 0.1192869292
## timbre_2_min              0.262841319  0.382043213 0.0402265624
## pitch                     0.294399713  0.336376154 0.0493222539
## timbre_5_min              0.195179188  0.264235231 0.0470870381
## timbre_6_max              0.220280830  0.230236051 0.0059826955
## energy                    0.282351251  0.329769434 0.5632351479
## timbre_1_max              0.147887015  0.319382009 0.0462136599
## timbre_10_max             0.108968439  0.172849789 0.0459537656
## timbre_8_min              0.102824593  0.337189672 0.0240473720
## timbre_2_max              0.154130354  0.147575396 0.0585797233
## timbre_9_min              0.247454393  0.480227550 0.0327041012
## loudness                  0.166991833  0.095301520 0.9058955144
## tempo_confidence          0.004713273  0.051643291 0.0774819401
## timbre_5_max              0.096891888  0.224828057 0.0268005031
## timbre_0_min              0.001905078  0.042752032 0.0474829253
## timesignature_confidence  0.034307220  0.073592966 0.1676558113
## timesignature             0.014355589  0.048818652 0.1000202527
## timbre_7_max              0.157315943  0.396808582 0.0131923760
## timbre_3_max              0.016934165  0.077538391 0.1384427330
## timbre_3_min              0.114592983  0.312779539 0.0475486231
## key                       0.003711362  0.003365663 0.0154776480
## key_confidence            0.003680199  0.022219296 0.0606462420
## timbre_4_min              0.154246437  0.080236736 0.0429582944
## timbre_10_min             0.009578378  0.188496107 0.1625037970
## timbre_1_min              0.286368842  0.375513659 0.0982142401
## tempo                     0.018156497  0.089338029 0.0006267798
##                          timbre_9_max timbre_11_max timbre_2_min
## timbre_6_min              0.350398795   0.287627661   0.33468254
## timbre_4_max              0.299157415   0.269329755   0.28099799
## timbre_11_min             0.197884104   0.100340776   0.21284636
## timbre_8_max              0.273580091   0.317062203   0.26284132
## timbre_7_min              0.480306074   0.212158521   0.38204321
## timbre_0_max              0.048101805   0.119286929   0.04022656
## timbre_9_max              0.000000000   0.148938415   0.33962450
## timbre_11_max             0.148938415   0.000000000   0.18760710
## timbre_2_min              0.339624497   0.187607098   0.00000000
## pitch                     0.218041638   0.200903150   0.35066686
## timbre_5_min              0.243147818   0.124021662   0.42052948
## timbre_6_max              0.233402549   0.293563661   0.24058235
## energy                    0.152416686   0.243295160   0.24300973
## timbre_1_max              0.349430332   0.176178429   0.24591835
## timbre_10_max             0.280516892   0.109054150   0.22991416
## timbre_8_min              0.229929550   0.301381265   0.25895382
## timbre_2_max              0.134203698   0.204646489   0.06892429
## timbre_9_min              0.311198690   0.232306316   0.44355187
## loudness                  0.026380233   0.173335027   0.07864783
## tempo_confidence          0.080879806   0.039039039   0.13353078
## timbre_5_max              0.393712297   0.064908698   0.24964052
## timbre_0_min              0.010839078   0.061334471   0.13387939
## timesignature_confidence  0.068768307   0.031432484   0.11076993
## timesignature             0.035693641   0.002586617   0.03971574
## timbre_7_max              0.367655592   0.123413462   0.32996685
## timbre_3_max              0.151310307   0.002925378   0.29105273
## timbre_3_min              0.326593257   0.091720171   0.21467925
## key                       0.008604285   0.017008236   0.03457335
## key_confidence            0.024188984   0.032322936   0.01670375
## timbre_4_min              0.123143414   0.131563583   0.17753915
## timbre_10_min             0.259533956   0.009378114   0.28249002
## timbre_1_min              0.187687648   0.203375969   0.32539515
## tempo                     0.049463498   0.012129520   0.04339832
##                               pitch timbre_5_min timbre_6_max      energy
## timbre_6_min             0.27804672  0.230666903  0.109892063 0.294757232
## timbre_4_max             0.30210668  0.244348086  0.299433795 0.289538100
## timbre_11_min            0.26153897  0.205150079  0.283300815 0.066856448
## timbre_8_max             0.29439971  0.195179188  0.220280830 0.282351251
## timbre_7_min             0.33637615  0.264235231  0.230236051 0.329769434
## timbre_0_max             0.04932225  0.047087038  0.005982696 0.563235148
## timbre_9_max             0.21804164  0.243147818  0.233402549 0.152416686
## timbre_11_max            0.20090315  0.124021662  0.293563661 0.243295160
## timbre_2_min             0.35066686  0.420529476  0.240582350 0.243009734
## pitch                    0.00000000  0.223960194  0.219001377 0.312262657
## timbre_5_min             0.22396019  0.000000000  0.226504870 0.036369274
## timbre_6_max             0.21900138  0.226504870  0.000000000 0.098479283
## energy                   0.31226266  0.036369274  0.098479283 0.000000000
## timbre_1_max             0.09810809  0.191875077  0.166754561 0.002001210
## timbre_10_max            0.08725314  0.184676809  0.089273384 0.022486279
## timbre_8_min             0.22939066  0.218309873  0.356422030 0.182794170
## timbre_2_max             0.06848500  0.089580677  0.367687159 0.013518960
## timbre_9_min             0.28790551  0.416216275  0.248575771 0.317943097
## loudness                 0.05656993  0.059089992  0.005232245 0.739906708
## tempo_confidence         0.05232348  0.108568422  0.062042883 0.151835896
## timbre_5_max             0.10359583  0.144038997  0.156548395 0.041564526
## timbre_0_min             0.05818190  0.030193786  0.006021398 0.090120314
## timesignature_confidence 0.09687713  0.111815829  0.076600503 0.105931843
## timesignature            0.09444193  0.051730222  0.055446717 0.086373400
## timbre_7_max             0.18438616  0.259243709  0.195604792 0.172629495
## timbre_3_max             0.03334388  0.406337856  0.082302782 0.160216580
## timbre_3_min             0.13616712  0.092879786  0.186143204 0.153648190
## key                      0.01145964  0.023763318  0.032824721 0.006268243
## key_confidence           0.08485858  0.021182571  0.004471869 0.055377445
## timbre_4_min             0.10820600  0.138930408  0.181223606 0.039110291
## timbre_10_min            0.01886741  0.125766699  0.094548646 0.108898889
## timbre_1_min             0.39195512  0.185315298  0.200257312 0.484662808
## tempo                    0.03859802  0.008690528  0.017609126 0.154989603
##                          timbre_1_max timbre_10_max timbre_8_min
## timbre_6_min               0.30491475   0.174794261  0.280754431
## timbre_4_max               0.31622699   0.104252143  0.391282450
## timbre_11_min              0.16563865   0.105775522  0.288524969
## timbre_8_max               0.14788701   0.108968439  0.102824593
## timbre_7_min               0.31938201   0.172849789  0.337189672
## timbre_0_max               0.04621366   0.045953766  0.024047372
## timbre_9_max               0.34943033   0.280516892  0.229929550
## timbre_11_max              0.17617843   0.109054150  0.301381265
## timbre_2_min               0.24591835   0.229914156  0.258953817
## pitch                      0.09810809   0.087253145  0.229390664
## timbre_5_min               0.19187508   0.184676809  0.218309873
## timbre_6_max               0.16675456   0.089273384  0.356422030
## energy                     0.00200121   0.022486279  0.182794170
## timbre_1_max               0.00000000   0.145669916  0.311733156
## timbre_10_max              0.14566992   0.000000000  0.089497147
## timbre_8_min               0.31173316   0.089497147  0.000000000
## timbre_2_max               0.28261858   0.076449437  0.279084929
## timbre_9_min               0.20999204   0.168599300  0.354235603
## loudness                   0.04843218   0.092053468  0.072235220
## tempo_confidence           0.04626091   0.082235706  0.042807148
## timbre_5_max               0.15094184   0.342166572  0.087237336
## timbre_0_min               0.08527484   0.044939690  0.060985110
## timesignature_confidence   0.04757327   0.024220307  0.079158394
## timesignature              0.02304274   0.024450016  0.050334757
## timbre_7_max               0.22382410   0.190467297  0.229674385
## timbre_3_max               0.14297015   0.179598318  0.045053702
## timbre_3_min               0.09181731   0.071899263  0.104275059
## key                        0.00835810   0.012914541  0.019667315
## key_confidence             0.04455801   0.014574252  0.005621326
## timbre_4_min               0.15063941   0.105485194  0.231026499
## timbre_10_min              0.20307467   0.189783690  0.080554935
## timbre_1_min               0.05414452   0.020543933  0.219176282
## tempo                      0.04376397   0.007268116  0.071572973
##                          timbre_2_max timbre_9_min    loudness
## timbre_6_min             0.2302414573   0.35086071 0.120855271
## timbre_4_max             0.3956877151   0.37746369 0.093376576
## timbre_11_min            0.2532807536   0.18977980 0.037403437
## timbre_8_max             0.1541303543   0.24745439 0.166991833
## timbre_7_min             0.1475753958   0.48022755 0.095301520
## timbre_0_max             0.0585797233   0.03270410 0.905895514
## timbre_9_max             0.1342036983   0.31119869 0.026380233
## timbre_11_max            0.2046464888   0.23230632 0.173335027
## timbre_2_min             0.0689242891   0.44355187 0.078647829
## pitch                    0.0684850040   0.28790551 0.056569934
## timbre_5_min             0.0895806765   0.41621628 0.059089992
## timbre_6_max             0.3676871592   0.24857577 0.005232245
## energy                   0.0135189596   0.31794310 0.739906708
## timbre_1_max             0.2826185842   0.20999204 0.048432176
## timbre_10_max            0.0764494369   0.16859930 0.092053468
## timbre_8_min             0.2790849287   0.35423560 0.072235220
## timbre_2_max             0.0000000000   0.11535359 0.052881838
## timbre_9_min             0.1153535855   0.00000000 0.099819479
## loudness                 0.0528818381   0.09981948 0.000000000
## tempo_confidence         0.0327553005   0.03285915 0.142882827
## timbre_5_max             0.0379334991   0.19928533 0.068169858
## timbre_0_min             0.1071360022   0.10070341 0.009399084
## timesignature_confidence 0.0018110808   0.05111407 0.165697849
## timesignature            0.0197393819   0.01465327 0.105771537
## timbre_7_max             0.0948650045   0.43881220 0.008656228
## timbre_3_max             0.0698456281   0.26829132 0.207245886
## timbre_3_min             0.0733918083   0.18491055 0.016158484
## key                      0.0182015842   0.02025505 0.007781500
## key_confidence           0.0005591351   0.01834614 0.019482412
## timbre_4_min             0.0888438468   0.10583691 0.003363140
## timbre_10_min            0.0691323664   0.13245828 0.211982830
## timbre_1_min             0.0355838445   0.44636549 0.260369254
## tempo                    0.0136902058   0.07339777 0.048535038
##                          tempo_confidence timbre_5_max timbre_0_min
## timbre_6_min                  0.017860800  0.200404407  0.048253824
## timbre_4_max                  0.040958171  0.109963011  0.044692613
## timbre_11_min                 0.093264502  0.126617896  0.011006016
## timbre_8_max                  0.004713273  0.096891888  0.001905078
## timbre_7_min                  0.051643291  0.224828057  0.042752032
## timbre_0_max                  0.077481940  0.026800503  0.047482925
## timbre_9_max                  0.080879806  0.393712297  0.010839078
## timbre_11_max                 0.039039039  0.064908698  0.061334471
## timbre_2_min                  0.133530783  0.249640519  0.133879387
## pitch                         0.052323478  0.103595831  0.058181899
## timbre_5_min                  0.108568422  0.144038997  0.030193786
## timbre_6_max                  0.062042883  0.156548395  0.006021398
## energy                        0.151835896  0.041564526  0.090120314
## timbre_1_max                  0.046260907  0.150941837  0.085274839
## timbre_10_max                 0.082235706  0.342166572  0.044939690
## timbre_8_min                  0.042807148  0.087237336  0.060985110
## timbre_2_max                  0.032755301  0.037933499  0.107136002
## timbre_9_min                  0.032859147  0.199285332  0.100703414
## loudness                      0.142882827  0.068169858  0.009399084
## tempo_confidence              0.000000000  0.086076636  0.031845980
## timbre_5_max                  0.086076636  0.000000000  0.024819207
## timbre_0_min                  0.031845980  0.024819207  0.000000000
## timesignature_confidence      0.328598944  0.050928400  0.026330389
## timesignature                 0.163705257  0.031055225  0.013387463
## timbre_7_max                  0.035375496  0.214537373  0.094520663
## timbre_3_max                  0.032902517  0.194677953  0.082459551
## timbre_3_min                  0.013770331  0.283879738  0.022874423
## key                           0.013342528  0.008153236  0.008790121
## key_confidence                0.013629201  0.020663866  0.044227848
## timbre_4_min                  0.002233332  0.118015402  0.020810701
## timbre_10_min                 0.043191735  0.301989938  0.041833097
## timbre_1_min                  0.049810469  0.049218571  0.107282699
## tempo                         0.023280660  0.009726892  0.021308733
##                          timesignature_confidence timesignature
## timbre_6_min                          0.031361625   0.029081140
## timbre_4_max                          0.080582500   0.038111591
## timbre_11_min                         0.073911622   0.071138001
## timbre_8_max                          0.034307220   0.014355589
## timbre_7_min                          0.073592966   0.048818652
## timbre_0_max                          0.167655811   0.100020253
## timbre_9_max                          0.068768307   0.035693641
## timbre_11_max                         0.031432484   0.002586617
## timbre_2_min                          0.110769925   0.039715742
## pitch                                 0.096877126   0.094441934
## timbre_5_min                          0.111815829   0.051730222
## timbre_6_max                          0.076600503   0.055446717
## energy                                0.105931843   0.086373400
## timbre_1_max                          0.047573274   0.023042736
## timbre_10_max                         0.024220307   0.024450016
## timbre_8_min                          0.079158394   0.050334757
## timbre_2_max                          0.001811081   0.019739382
## timbre_9_min                          0.051114075   0.014653267
## loudness                              0.165697849   0.105771537
## tempo_confidence                      0.328598944   0.163705257
## timbre_5_max                          0.050928400   0.031055225
## timbre_0_min                          0.026330389   0.013387463
## timesignature_confidence              0.000000000   0.343982155
## timesignature                         0.343982155   0.000000000
## timbre_7_max                          0.033254180   0.002040133
## timbre_3_max                          0.004536818   0.022124539
## timbre_3_min                          0.022642567   0.006046027
## key                                   0.006794214   0.017572596
## key_confidence                        0.016430285   0.015918168
## timbre_4_min                          0.020021944   0.004021022
## timbre_10_min                         0.047834679   0.008729925
## timbre_1_min                          0.024430815   0.016925000
## tempo                                 0.029783675   0.048710910
##                          timbre_7_max timbre_3_max timbre_3_min
## timbre_6_min              0.296872849  0.153038250 0.1820907729
## timbre_4_max              0.243897425  0.009126047 0.1104430347
## timbre_11_min             0.121150310  0.020697075 0.0633110132
## timbre_8_max              0.157315943  0.016934165 0.1145929826
## timbre_7_min              0.396808582  0.077538391 0.3127795392
## timbre_0_max              0.013192376  0.138442733 0.0475486231
## timbre_9_max              0.367655592  0.151310307 0.3265932568
## timbre_11_max             0.123413462  0.002925378 0.0917201714
## timbre_2_min              0.329966846  0.291052730 0.2146792494
## pitch                     0.184386155  0.033343878 0.1361671151
## timbre_5_min              0.259243709  0.406337856 0.0928797862
## timbre_6_max              0.195604792  0.082302782 0.1861432039
## energy                    0.172629495  0.160216580 0.1536481903
## timbre_1_max              0.223824104  0.142970150 0.0918173123
## timbre_10_max             0.190467297  0.179598318 0.0718992627
## timbre_8_min              0.229674385  0.045053702 0.1042750586
## timbre_2_max              0.094865005  0.069845628 0.0733918083
## timbre_9_min              0.438812202  0.268291318 0.1849105474
## loudness                  0.008656228  0.207245886 0.0161584843
## tempo_confidence          0.035375496  0.032902517 0.0137703313
## timbre_5_max              0.214537373  0.194677953 0.2838797383
## timbre_0_min              0.094520663  0.082459551 0.0228744234
## timesignature_confidence  0.033254180  0.004536818 0.0226425671
## timesignature             0.002040133  0.022124539 0.0060460266
## timbre_7_max              0.000000000  0.340934695 0.2000254906
## timbre_3_max              0.340934695  0.000000000 0.1323063227
## timbre_3_min              0.200025491  0.132306323 0.0000000000
## key                       0.005510934  0.010024050 0.0080427657
## key_confidence            0.003676716  0.049084134 0.0003020344
## timbre_4_min              0.084025763  0.106272159 0.0599235058
## timbre_10_min             0.248894049  0.509846782 0.2514500287
## timbre_1_min              0.275511097  0.109976920 0.1324010792
## tempo                     0.075725817  0.005910305 0.0479623596
##                                  key key_confidence timbre_4_min
## timbre_6_min             0.017555376   0.0189442544  0.070407925
## timbre_4_max             0.005219950   0.0274720602  0.136746164
## timbre_11_min            0.007080471   0.0332622220  0.210352846
## timbre_8_max             0.003711362   0.0036801985  0.154246437
## timbre_7_min             0.003365663   0.0222192962  0.080236736
## timbre_0_max             0.015477648   0.0606462420  0.042958294
## timbre_9_max             0.008604285   0.0241889840  0.123143414
## timbre_11_max            0.017008236   0.0323229361  0.131563583
## timbre_2_min             0.034573353   0.0167037493  0.177539148
## pitch                    0.011459639   0.0848585775  0.108205998
## timbre_5_min             0.023763318   0.0211825706  0.138930408
## timbre_6_max             0.032824721   0.0044718691  0.181223606
## energy                   0.006268243   0.0553774453  0.039110291
## timbre_1_max             0.008358100   0.0445580080  0.150639406
## timbre_10_max            0.012914541   0.0145742522  0.105485194
## timbre_8_min             0.019667315   0.0056213263  0.231026499
## timbre_2_max             0.018201584   0.0005591351  0.088843847
## timbre_9_min             0.020255052   0.0183461407  0.105836913
## loudness                 0.007781500   0.0194824125  0.003363140
## tempo_confidence         0.013342528   0.0136292011  0.002233332
## timbre_5_max             0.008153236   0.0206638661  0.118015402
## timbre_0_min             0.008790121   0.0442278483  0.020810701
## timesignature_confidence 0.006794214   0.0164302851  0.020021944
## timesignature            0.017572596   0.0159181676  0.004021022
## timbre_7_max             0.005510934   0.0036767165  0.084025763
## timbre_3_max             0.010024050   0.0490841335  0.106272159
## timbre_3_min             0.008042766   0.0003020344  0.059923506
## key                      0.000000000   0.0435718053  0.027567496
## key_confidence           0.043571805   0.0000000000  0.024946146
## timbre_4_min             0.027567496   0.0249461462  0.000000000
## timbre_10_min            0.007024254   0.0604936404  0.114621536
## timbre_1_min             0.010649185   0.0757591439  0.026381988
## tempo                    0.018874310   0.0709959251  0.018817596
##                          timbre_10_min timbre_1_min        tempo
## timbre_6_min               0.161633164   0.20827637 0.0777248783
## timbre_4_max               0.043954487   0.35244899 0.0849947746
## timbre_11_min              0.064520244   0.13595888 0.0031223853
## timbre_8_max               0.009578378   0.28636884 0.0181564969
## timbre_7_min               0.188496107   0.37551366 0.0893380293
## timbre_0_max               0.162503797   0.09821424 0.0006267798
## timbre_9_max               0.259533956   0.18768765 0.0494634981
## timbre_11_max              0.009378114   0.20337597 0.0121295203
## timbre_2_min               0.282490023   0.32539515 0.0433983220
## pitch                      0.018867411   0.39195512 0.0385980153
## timbre_5_min               0.125766699   0.18531530 0.0086905285
## timbre_6_max               0.094548646   0.20025731 0.0176091258
## energy                     0.108898889   0.48466281 0.1549896030
## timbre_1_max               0.203074667   0.05414452 0.0437639708
## timbre_10_max              0.189783690   0.02054393 0.0072681162
## timbre_8_min               0.080554935   0.21917628 0.0715729733
## timbre_2_max               0.069132366   0.03558384 0.0136902058
## timbre_9_min               0.132458282   0.44636549 0.0733977686
## loudness                   0.211982830   0.26036925 0.0485350385
## tempo_confidence           0.043191735   0.04981047 0.0232806600
## timbre_5_max               0.301989938   0.04921857 0.0097268920
## timbre_0_min               0.041833097   0.10728270 0.0213087331
## timesignature_confidence   0.047834679   0.02443081 0.0297836750
## timesignature              0.008729925   0.01692500 0.0487109099
## timbre_7_max               0.248894049   0.27551110 0.0757258169
## timbre_3_max               0.509846782   0.10997692 0.0059103046
## timbre_3_min               0.251450029   0.13240108 0.0479623596
## key                        0.007024254   0.01064919 0.0188743099
## key_confidence             0.060493640   0.07575914 0.0709959251
## timbre_4_min               0.114621536   0.02638199 0.0188175962
## timbre_10_min              0.000000000   0.05979506 0.0313747819
## timbre_1_min               0.059795059   0.00000000 0.0794040880
## tempo                      0.031374782   0.07940409 0.0000000000
## [1] "cor(timbre_0_max, loudness)=0.9059"
```

![](songs_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Top10, timbre_0_max)=-0.1522"
## [1] "cor(Top10, loudness)=-0.0865"
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping loudness as a feature
```

![](songs_files/figure-html/remove_correlated_features-2.png) 

```
##                                                id        cor.y   cor.y.abs
## timbre_6_min                         timbre_6_min -0.216378268 0.216378268
## timbre_4_max                         timbre_4_max  0.197034666 0.197034666
## timbre_11_min                       timbre_11_min -0.193049686 0.193049686
## timbre_8_max                         timbre_8_max  0.162722687 0.162722687
## timbre_7_min                         timbre_7_min -0.155645339 0.155645339
## timbre_0_max                         timbre_0_max -0.152171355 0.152171355
## timbre_9_max                         timbre_9_max  0.145756048 0.145756048
## timbre_11_max                       timbre_11_max  0.143718363 0.143718363
## timbre_2_min                         timbre_2_min -0.136085935 0.136085935
## pitch                                       pitch -0.135723510 0.135723510
## timbre_5_min                         timbre_5_min -0.123590779 0.123590779
## timbre_6_max                         timbre_6_max  0.123432076 0.123432076
## energy                                     energy -0.120897670 0.120897670
## timbre_1_max                         timbre_1_max  0.106338252 0.106338252
## timbre_10_max                       timbre_10_max  0.104441775 0.104441775
## timbre_8_min                         timbre_8_min -0.101131084 0.101131084
## timbre_2_max                         timbre_2_max  0.096545551 0.096545551
## timbre_9_min                         timbre_9_min -0.088167503 0.088167503
## tempo_confidence                 tempo_confidence  0.086315020 0.086315020
## timbre_5_max                         timbre_5_max  0.076923330 0.076923330
## timbre_0_min                         timbre_0_min  0.068825164 0.068825164
## timesignature_confidence timesignature_confidence  0.063023674 0.063023674
## timesignature                       timesignature  0.046630910 0.046630910
## timbre_7_max                         timbre_7_max  0.045966630 0.045966630
## timbre_3_max                         timbre_3_max -0.026744876 0.026744876
## timbre_3_min                         timbre_3_min -0.026442066 0.026442066
## key                                           key  0.025326063 0.025326063
## key_confidence                     key_confidence  0.010021403 0.010021403
## timbre_4_min                         timbre_4_min -0.006010064 0.006010064
## timbre_10_min                       timbre_10_min  0.005257590 0.005257590
## timbre_1_min                         timbre_1_min  0.003659089 0.003659089
## tempo                                       tempo -0.003577709 0.003577709
##                          timbre_6_min timbre_4_max timbre_11_min
## timbre_6_min               1.00000000 -0.455025649   0.273390352
## timbre_4_max              -0.45502565  1.000000000  -0.302295478
## timbre_11_min              0.27339035 -0.302295478   1.000000000
## timbre_8_max              -0.34336035  0.295179151  -0.272093182
## timbre_7_min               0.45337904 -0.416885588   0.243882172
## timbre_0_max               0.06960243 -0.013662234  -0.027117957
## timbre_9_max              -0.35039880  0.299157415  -0.197884104
## timbre_11_max             -0.28762766  0.269329755  -0.100340776
## timbre_2_min               0.33468254 -0.280997994   0.212846357
## pitch                      0.27804672 -0.302106679   0.261538974
## timbre_5_min               0.23066690 -0.244348086   0.205150079
## timbre_6_max              -0.10989206  0.299433795  -0.283300815
## energy                     0.29475723 -0.289538100   0.066856448
## timbre_1_max              -0.30491475  0.316226992  -0.165638653
## timbre_10_max             -0.17479426  0.104252143  -0.105775522
## timbre_8_min               0.28075443 -0.391282450   0.288524969
## timbre_2_max              -0.23024146  0.395687715  -0.253280754
## timbre_9_min               0.35086071 -0.377463693   0.189779797
## tempo_confidence          -0.01786080  0.040958171  -0.093264502
## timbre_5_max              -0.20040441  0.109963011  -0.126617896
## timbre_0_min               0.04825382 -0.044692613   0.011006016
## timesignature_confidence  -0.03136163  0.080582500  -0.073911622
## timesignature             -0.02908114  0.038111591  -0.071138001
## timbre_7_max              -0.29687285  0.243897425  -0.121150310
## timbre_3_max              -0.15303825 -0.009126047  -0.020697075
## timbre_3_min               0.18209077 -0.110443035   0.063311013
## key                       -0.01755538  0.005219950   0.007080471
## key_confidence            -0.01894425  0.027472060  -0.033262222
## timbre_4_min               0.07040792  0.136746164   0.210352846
## timbre_10_min              0.16163316 -0.043954487   0.064520244
## timbre_1_min               0.20827637 -0.352448987   0.135958877
## tempo                      0.07772488 -0.084994775   0.003122385
##                          timbre_8_max timbre_7_min  timbre_0_max
## timbre_6_min             -0.343360348  0.453379045  0.0696024349
## timbre_4_max              0.295179151 -0.416885588 -0.0136622340
## timbre_11_min            -0.272093182  0.243882172 -0.0271179570
## timbre_8_max              1.000000000 -0.311466153 -0.1395411785
## timbre_7_min             -0.311466153  1.000000000  0.0378223542
## timbre_0_max             -0.139541178  0.037822354  1.0000000000
## timbre_9_max              0.273580091 -0.480306074 -0.0481018049
## timbre_11_max             0.317062203 -0.212158521 -0.1192869292
## timbre_2_min             -0.262841319  0.382043213  0.0402265624
## pitch                    -0.294399713  0.336376154 -0.0493222539
## timbre_5_min             -0.195179188  0.264235231 -0.0470870381
## timbre_6_max              0.220280830 -0.230236051  0.0059826955
## energy                   -0.282351251  0.329769434  0.5632351479
## timbre_1_max              0.147887015 -0.319382009  0.0462136599
## timbre_10_max             0.108968439 -0.172849789  0.0459537656
## timbre_8_min             -0.102824593  0.337189672  0.0240473720
## timbre_2_max              0.154130354 -0.147575396  0.0585797233
## timbre_9_min             -0.247454393  0.480227550  0.0327041012
## tempo_confidence         -0.004713273 -0.051643291  0.0774819401
## timbre_5_max              0.096891888 -0.224828057  0.0268005031
## timbre_0_min              0.001905078  0.042752032 -0.0474829253
## timesignature_confidence -0.034307220 -0.073592966  0.1676558113
## timesignature             0.014355589 -0.048818652  0.1000202527
## timbre_7_max              0.157315943 -0.396808582  0.0131923760
## timbre_3_max             -0.016934165 -0.077538391  0.1384427330
## timbre_3_min             -0.114592983  0.312779539 -0.0475486231
## key                       0.003711362  0.003365663 -0.0154776480
## key_confidence            0.003680199 -0.022219296  0.0606462420
## timbre_4_min             -0.154246437  0.080236736  0.0429582944
## timbre_10_min            -0.009578378  0.188496107 -0.1625037970
## timbre_1_min             -0.286368842  0.375513659  0.0982142401
## tempo                    -0.018156497  0.089338029  0.0006267798
##                          timbre_9_max timbre_11_max timbre_2_min
## timbre_6_min             -0.350398795  -0.287627661   0.33468254
## timbre_4_max              0.299157415   0.269329755  -0.28099799
## timbre_11_min            -0.197884104  -0.100340776   0.21284636
## timbre_8_max              0.273580091   0.317062203  -0.26284132
## timbre_7_min             -0.480306074  -0.212158521   0.38204321
## timbre_0_max             -0.048101805  -0.119286929   0.04022656
## timbre_9_max              1.000000000   0.148938415  -0.33962450
## timbre_11_max             0.148938415   1.000000000  -0.18760710
## timbre_2_min             -0.339624497  -0.187607098   1.00000000
## pitch                    -0.218041638  -0.200903150   0.35066686
## timbre_5_min             -0.243147818  -0.124021662   0.42052948
## timbre_6_max              0.233402549   0.293563661  -0.24058235
## energy                   -0.152416686  -0.243295160   0.24300973
## timbre_1_max              0.349430332   0.176178429  -0.24591835
## timbre_10_max             0.280516892   0.109054150  -0.22991416
## timbre_8_min             -0.229929550  -0.301381265   0.25895382
## timbre_2_max              0.134203698   0.204646489   0.06892429
## timbre_9_min             -0.311198690  -0.232306316   0.44355187
## tempo_confidence          0.080879806  -0.039039039  -0.13353078
## timbre_5_max              0.393712297   0.064908698  -0.24964052
## timbre_0_min              0.010839078  -0.061334471   0.13387939
## timesignature_confidence  0.068768307  -0.031432484  -0.11076993
## timesignature             0.035693641  -0.002586617  -0.03971574
## timbre_7_max              0.367655592   0.123413462  -0.32996685
## timbre_3_max              0.151310307  -0.002925378  -0.29105273
## timbre_3_min             -0.326593257  -0.091720171   0.21467925
## key                       0.008604285   0.017008236  -0.03457335
## key_confidence           -0.024188984   0.032322936   0.01670375
## timbre_4_min             -0.123143414  -0.131563583   0.17753915
## timbre_10_min            -0.259533956   0.009378114   0.28249002
## timbre_1_min             -0.187687648  -0.203375969   0.32539515
## tempo                    -0.049463498  -0.012129520   0.04339832
##                                pitch timbre_5_min timbre_6_max
## timbre_6_min              0.27804672  0.230666903 -0.109892063
## timbre_4_max             -0.30210668 -0.244348086  0.299433795
## timbre_11_min             0.26153897  0.205150079 -0.283300815
## timbre_8_max             -0.29439971 -0.195179188  0.220280830
## timbre_7_min              0.33637615  0.264235231 -0.230236051
## timbre_0_max             -0.04932225 -0.047087038  0.005982696
## timbre_9_max             -0.21804164 -0.243147818  0.233402549
## timbre_11_max            -0.20090315 -0.124021662  0.293563661
## timbre_2_min              0.35066686  0.420529476 -0.240582350
## pitch                     1.00000000  0.223960194 -0.219001377
## timbre_5_min              0.22396019  1.000000000 -0.226504870
## timbre_6_max             -0.21900138 -0.226504870  1.000000000
## energy                    0.31226266  0.036369274 -0.098479283
## timbre_1_max             -0.09810809 -0.191875077  0.166754561
## timbre_10_max            -0.08725314 -0.184676809  0.089273384
## timbre_8_min              0.22939066  0.218309873 -0.356422030
## timbre_2_max             -0.06848500 -0.089580677  0.367687159
## timbre_9_min              0.28790551  0.416216275 -0.248575771
## tempo_confidence         -0.05232348 -0.108568422  0.062042883
## timbre_5_max             -0.10359583 -0.144038997  0.156548395
## timbre_0_min              0.05818190  0.030193786  0.006021398
## timesignature_confidence -0.09687713 -0.111815829  0.076600503
## timesignature            -0.09444193 -0.051730222  0.055446717
## timbre_7_max             -0.18438616 -0.259243709  0.195604792
## timbre_3_max              0.03334388 -0.406337856  0.082302782
## timbre_3_min              0.13616712  0.092879786 -0.186143204
## key                       0.01145964 -0.023763318  0.032824721
## key_confidence           -0.08485858  0.021182571 -0.004471869
## timbre_4_min              0.10820600  0.138930408 -0.181223606
## timbre_10_min             0.01886741  0.125766699 -0.094548646
## timbre_1_min              0.39195512  0.185315298 -0.200257312
## tempo                     0.03859802  0.008690528 -0.017609126
##                                energy timbre_1_max timbre_10_max
## timbre_6_min              0.294757232  -0.30491475  -0.174794261
## timbre_4_max             -0.289538100   0.31622699   0.104252143
## timbre_11_min             0.066856448  -0.16563865  -0.105775522
## timbre_8_max             -0.282351251   0.14788701   0.108968439
## timbre_7_min              0.329769434  -0.31938201  -0.172849789
## timbre_0_max              0.563235148   0.04621366   0.045953766
## timbre_9_max             -0.152416686   0.34943033   0.280516892
## timbre_11_max            -0.243295160   0.17617843   0.109054150
## timbre_2_min              0.243009734  -0.24591835  -0.229914156
## pitch                     0.312262657  -0.09810809  -0.087253145
## timbre_5_min              0.036369274  -0.19187508  -0.184676809
## timbre_6_max             -0.098479283   0.16675456   0.089273384
## energy                    1.000000000   0.00200121   0.022486279
## timbre_1_max              0.002001210   1.00000000   0.145669916
## timbre_10_max             0.022486279   0.14566992   1.000000000
## timbre_8_min              0.182794170  -0.31173316  -0.089497147
## timbre_2_max              0.013518960   0.28261858   0.076449437
## timbre_9_min              0.317943097  -0.20999204  -0.168599300
## tempo_confidence          0.151835896   0.04626091   0.082235706
## timbre_5_max             -0.041564526   0.15094184   0.342166572
## timbre_0_min              0.090120314  -0.08527484  -0.044939690
## timesignature_confidence  0.105931843   0.04757327   0.024220307
## timesignature             0.086373400   0.02304274   0.024450016
## timbre_7_max             -0.172629495   0.22382410   0.190467297
## timbre_3_max              0.160216580   0.14297015   0.179598318
## timbre_3_min              0.153648190  -0.09181731  -0.071899263
## key                       0.006268243   0.00835810   0.012914541
## key_confidence           -0.055377445  -0.04455801  -0.014574252
## timbre_4_min             -0.039110291  -0.15063941  -0.105485194
## timbre_10_min            -0.108898889  -0.20307467  -0.189783690
## timbre_1_min              0.484662808   0.05414452  -0.020543933
## tempo                     0.154989603  -0.04376397  -0.007268116
##                          timbre_8_min  timbre_2_max timbre_9_min
## timbre_6_min              0.280754431 -0.2302414573   0.35086071
## timbre_4_max             -0.391282450  0.3956877151  -0.37746369
## timbre_11_min             0.288524969 -0.2532807536   0.18977980
## timbre_8_max             -0.102824593  0.1541303543  -0.24745439
## timbre_7_min              0.337189672 -0.1475753958   0.48022755
## timbre_0_max              0.024047372  0.0585797233   0.03270410
## timbre_9_max             -0.229929550  0.1342036983  -0.31119869
## timbre_11_max            -0.301381265  0.2046464888  -0.23230632
## timbre_2_min              0.258953817  0.0689242891   0.44355187
## pitch                     0.229390664 -0.0684850040   0.28790551
## timbre_5_min              0.218309873 -0.0895806765   0.41621628
## timbre_6_max             -0.356422030  0.3676871592  -0.24857577
## energy                    0.182794170  0.0135189596   0.31794310
## timbre_1_max             -0.311733156  0.2826185842  -0.20999204
## timbre_10_max            -0.089497147  0.0764494369  -0.16859930
## timbre_8_min              1.000000000 -0.2790849287   0.35423560
## timbre_2_max             -0.279084929  1.0000000000  -0.11535359
## timbre_9_min              0.354235603 -0.1153535855   1.00000000
## tempo_confidence         -0.042807148 -0.0327553005  -0.03285915
## timbre_5_max             -0.087237336  0.0379334991  -0.19928533
## timbre_0_min              0.060985110  0.1071360022   0.10070341
## timesignature_confidence -0.079158394 -0.0018110808  -0.05111407
## timesignature            -0.050334757  0.0197393819  -0.01465327
## timbre_7_max             -0.229674385  0.0948650045  -0.43881220
## timbre_3_max             -0.045053702  0.0698456281  -0.26829132
## timbre_3_min              0.104275059 -0.0733918083   0.18491055
## key                      -0.019667315  0.0182015842  -0.02025505
## key_confidence           -0.005621326  0.0005591351  -0.01834614
## timbre_4_min              0.231026499 -0.0888438468   0.10583691
## timbre_10_min             0.080554935 -0.0691323664   0.13245828
## timbre_1_min              0.219176282 -0.0355838445   0.44636549
## tempo                     0.071572973 -0.0136902058   0.07339777
##                          tempo_confidence timbre_5_max timbre_0_min
## timbre_6_min                 -0.017860800 -0.200404407  0.048253824
## timbre_4_max                  0.040958171  0.109963011 -0.044692613
## timbre_11_min                -0.093264502 -0.126617896  0.011006016
## timbre_8_max                 -0.004713273  0.096891888  0.001905078
## timbre_7_min                 -0.051643291 -0.224828057  0.042752032
## timbre_0_max                  0.077481940  0.026800503 -0.047482925
## timbre_9_max                  0.080879806  0.393712297  0.010839078
## timbre_11_max                -0.039039039  0.064908698 -0.061334471
## timbre_2_min                 -0.133530783 -0.249640519  0.133879387
## pitch                        -0.052323478 -0.103595831  0.058181899
## timbre_5_min                 -0.108568422 -0.144038997  0.030193786
## timbre_6_max                  0.062042883  0.156548395  0.006021398
## energy                        0.151835896 -0.041564526  0.090120314
## timbre_1_max                  0.046260907  0.150941837 -0.085274839
## timbre_10_max                 0.082235706  0.342166572 -0.044939690
## timbre_8_min                 -0.042807148 -0.087237336  0.060985110
## timbre_2_max                 -0.032755301  0.037933499  0.107136002
## timbre_9_min                 -0.032859147 -0.199285332  0.100703414
## tempo_confidence              1.000000000  0.086076636 -0.031845980
## timbre_5_max                  0.086076636  1.000000000 -0.024819207
## timbre_0_min                 -0.031845980 -0.024819207  1.000000000
## timesignature_confidence      0.328598944  0.050928400 -0.026330389
## timesignature                 0.163705257  0.031055225  0.013387463
## timbre_7_max                  0.035375496  0.214537373 -0.094520663
## timbre_3_max                  0.032902517  0.194677953 -0.082459551
## timbre_3_min                  0.013770331 -0.283879738 -0.022874423
## key                           0.013342528  0.008153236 -0.008790121
## key_confidence               -0.013629201 -0.020663866 -0.044227848
## timbre_4_min                  0.002233332 -0.118015402  0.020810701
## timbre_10_min                -0.043191735 -0.301989938  0.041833097
## timbre_1_min                  0.049810469 -0.049218571  0.107282699
## tempo                        -0.023280660 -0.009726892  0.021308733
##                          timesignature_confidence timesignature
## timbre_6_min                         -0.031361625  -0.029081140
## timbre_4_max                          0.080582500   0.038111591
## timbre_11_min                        -0.073911622  -0.071138001
## timbre_8_max                         -0.034307220   0.014355589
## timbre_7_min                         -0.073592966  -0.048818652
## timbre_0_max                          0.167655811   0.100020253
## timbre_9_max                          0.068768307   0.035693641
## timbre_11_max                        -0.031432484  -0.002586617
## timbre_2_min                         -0.110769925  -0.039715742
## pitch                                -0.096877126  -0.094441934
## timbre_5_min                         -0.111815829  -0.051730222
## timbre_6_max                          0.076600503   0.055446717
## energy                                0.105931843   0.086373400
## timbre_1_max                          0.047573274   0.023042736
## timbre_10_max                         0.024220307   0.024450016
## timbre_8_min                         -0.079158394  -0.050334757
## timbre_2_max                         -0.001811081   0.019739382
## timbre_9_min                         -0.051114075  -0.014653267
## tempo_confidence                      0.328598944   0.163705257
## timbre_5_max                          0.050928400   0.031055225
## timbre_0_min                         -0.026330389   0.013387463
## timesignature_confidence              1.000000000   0.343982155
## timesignature                         0.343982155   1.000000000
## timbre_7_max                          0.033254180   0.002040133
## timbre_3_max                          0.004536818  -0.022124539
## timbre_3_min                         -0.022642567   0.006046027
## key                                   0.006794214   0.017572596
## key_confidence                       -0.016430285  -0.015918168
## timbre_4_min                          0.020021944  -0.004021022
## timbre_10_min                        -0.047834679  -0.008729925
## timbre_1_min                         -0.024430815  -0.016925000
## tempo                                -0.029783675   0.048710910
##                          timbre_7_max timbre_3_max  timbre_3_min
## timbre_6_min             -0.296872849 -0.153038250  0.1820907729
## timbre_4_max              0.243897425 -0.009126047 -0.1104430347
## timbre_11_min            -0.121150310 -0.020697075  0.0633110132
## timbre_8_max              0.157315943 -0.016934165 -0.1145929826
## timbre_7_min             -0.396808582 -0.077538391  0.3127795392
## timbre_0_max              0.013192376  0.138442733 -0.0475486231
## timbre_9_max              0.367655592  0.151310307 -0.3265932568
## timbre_11_max             0.123413462 -0.002925378 -0.0917201714
## timbre_2_min             -0.329966846 -0.291052730  0.2146792494
## pitch                    -0.184386155  0.033343878  0.1361671151
## timbre_5_min             -0.259243709 -0.406337856  0.0928797862
## timbre_6_max              0.195604792  0.082302782 -0.1861432039
## energy                   -0.172629495  0.160216580  0.1536481903
## timbre_1_max              0.223824104  0.142970150 -0.0918173123
## timbre_10_max             0.190467297  0.179598318 -0.0718992627
## timbre_8_min             -0.229674385 -0.045053702  0.1042750586
## timbre_2_max              0.094865005  0.069845628 -0.0733918083
## timbre_9_min             -0.438812202 -0.268291318  0.1849105474
## tempo_confidence          0.035375496  0.032902517  0.0137703313
## timbre_5_max              0.214537373  0.194677953 -0.2838797383
## timbre_0_min             -0.094520663 -0.082459551 -0.0228744234
## timesignature_confidence  0.033254180  0.004536818 -0.0226425671
## timesignature             0.002040133 -0.022124539  0.0060460266
## timbre_7_max              1.000000000  0.340934695 -0.2000254906
## timbre_3_max              0.340934695  1.000000000 -0.1323063227
## timbre_3_min             -0.200025491 -0.132306323  1.0000000000
## key                       0.005510934  0.010024050 -0.0080427657
## key_confidence           -0.003676716 -0.049084134 -0.0003020344
## timbre_4_min             -0.084025763 -0.106272159  0.0599235058
## timbre_10_min            -0.248894049 -0.509846782  0.2514500287
## timbre_1_min             -0.275511097  0.109976920  0.1324010792
## tempo                    -0.075725817 -0.005910305  0.0479623596
##                                   key key_confidence timbre_4_min
## timbre_6_min             -0.017555376  -0.0189442544  0.070407925
## timbre_4_max              0.005219950   0.0274720602  0.136746164
## timbre_11_min             0.007080471  -0.0332622220  0.210352846
## timbre_8_max              0.003711362   0.0036801985 -0.154246437
## timbre_7_min              0.003365663  -0.0222192962  0.080236736
## timbre_0_max             -0.015477648   0.0606462420  0.042958294
## timbre_9_max              0.008604285  -0.0241889840 -0.123143414
## timbre_11_max             0.017008236   0.0323229361 -0.131563583
## timbre_2_min             -0.034573353   0.0167037493  0.177539148
## pitch                     0.011459639  -0.0848585775  0.108205998
## timbre_5_min             -0.023763318   0.0211825706  0.138930408
## timbre_6_max              0.032824721  -0.0044718691 -0.181223606
## energy                    0.006268243  -0.0553774453 -0.039110291
## timbre_1_max              0.008358100  -0.0445580080 -0.150639406
## timbre_10_max             0.012914541  -0.0145742522 -0.105485194
## timbre_8_min             -0.019667315  -0.0056213263  0.231026499
## timbre_2_max              0.018201584   0.0005591351 -0.088843847
## timbre_9_min             -0.020255052  -0.0183461407  0.105836913
## tempo_confidence          0.013342528  -0.0136292011  0.002233332
## timbre_5_max              0.008153236  -0.0206638661 -0.118015402
## timbre_0_min             -0.008790121  -0.0442278483  0.020810701
## timesignature_confidence  0.006794214  -0.0164302851  0.020021944
## timesignature             0.017572596  -0.0159181676 -0.004021022
## timbre_7_max              0.005510934  -0.0036767165 -0.084025763
## timbre_3_max              0.010024050  -0.0490841335 -0.106272159
## timbre_3_min             -0.008042766  -0.0003020344  0.059923506
## key                       1.000000000  -0.0435718053 -0.027567496
## key_confidence           -0.043571805   1.0000000000  0.024946146
## timbre_4_min             -0.027567496   0.0249461462  1.000000000
## timbre_10_min             0.007024254   0.0604936404  0.114621536
## timbre_1_min              0.010649185  -0.0757591439  0.026381988
## tempo                     0.018874310   0.0709959251 -0.018817596
##                          timbre_10_min timbre_1_min         tempo
## timbre_6_min               0.161633164   0.20827637  0.0777248783
## timbre_4_max              -0.043954487  -0.35244899 -0.0849947746
## timbre_11_min              0.064520244   0.13595888  0.0031223853
## timbre_8_max              -0.009578378  -0.28636884 -0.0181564969
## timbre_7_min               0.188496107   0.37551366  0.0893380293
## timbre_0_max              -0.162503797   0.09821424  0.0006267798
## timbre_9_max              -0.259533956  -0.18768765 -0.0494634981
## timbre_11_max              0.009378114  -0.20337597 -0.0121295203
## timbre_2_min               0.282490023   0.32539515  0.0433983220
## pitch                      0.018867411   0.39195512  0.0385980153
## timbre_5_min               0.125766699   0.18531530  0.0086905285
## timbre_6_max              -0.094548646  -0.20025731 -0.0176091258
## energy                    -0.108898889   0.48466281  0.1549896030
## timbre_1_max              -0.203074667   0.05414452 -0.0437639708
## timbre_10_max             -0.189783690  -0.02054393 -0.0072681162
## timbre_8_min               0.080554935   0.21917628  0.0715729733
## timbre_2_max              -0.069132366  -0.03558384 -0.0136902058
## timbre_9_min               0.132458282   0.44636549  0.0733977686
## tempo_confidence          -0.043191735   0.04981047 -0.0232806600
## timbre_5_max              -0.301989938  -0.04921857 -0.0097268920
## timbre_0_min               0.041833097   0.10728270  0.0213087331
## timesignature_confidence  -0.047834679  -0.02443081 -0.0297836750
## timesignature             -0.008729925  -0.01692500  0.0487109099
## timbre_7_max              -0.248894049  -0.27551110 -0.0757258169
## timbre_3_max              -0.509846782   0.10997692 -0.0059103046
## timbre_3_min               0.251450029   0.13240108  0.0479623596
## key                        0.007024254   0.01064919  0.0188743099
## key_confidence             0.060493640  -0.07575914  0.0709959251
## timbre_4_min               0.114621536   0.02638199 -0.0188175962
## timbre_10_min              1.000000000  -0.05979506  0.0313747819
## timbre_1_min              -0.059795059   1.00000000  0.0794040880
## tempo                      0.031374782   0.07940409  1.0000000000
##                          timbre_6_min timbre_4_max timbre_11_min
## timbre_6_min               0.00000000  0.455025649   0.273390352
## timbre_4_max               0.45502565  0.000000000   0.302295478
## timbre_11_min              0.27339035  0.302295478   0.000000000
## timbre_8_max               0.34336035  0.295179151   0.272093182
## timbre_7_min               0.45337904  0.416885588   0.243882172
## timbre_0_max               0.06960243  0.013662234   0.027117957
## timbre_9_max               0.35039880  0.299157415   0.197884104
## timbre_11_max              0.28762766  0.269329755   0.100340776
## timbre_2_min               0.33468254  0.280997994   0.212846357
## pitch                      0.27804672  0.302106679   0.261538974
## timbre_5_min               0.23066690  0.244348086   0.205150079
## timbre_6_max               0.10989206  0.299433795   0.283300815
## energy                     0.29475723  0.289538100   0.066856448
## timbre_1_max               0.30491475  0.316226992   0.165638653
## timbre_10_max              0.17479426  0.104252143   0.105775522
## timbre_8_min               0.28075443  0.391282450   0.288524969
## timbre_2_max               0.23024146  0.395687715   0.253280754
## timbre_9_min               0.35086071  0.377463693   0.189779797
## tempo_confidence           0.01786080  0.040958171   0.093264502
## timbre_5_max               0.20040441  0.109963011   0.126617896
## timbre_0_min               0.04825382  0.044692613   0.011006016
## timesignature_confidence   0.03136163  0.080582500   0.073911622
## timesignature              0.02908114  0.038111591   0.071138001
## timbre_7_max               0.29687285  0.243897425   0.121150310
## timbre_3_max               0.15303825  0.009126047   0.020697075
## timbre_3_min               0.18209077  0.110443035   0.063311013
## key                        0.01755538  0.005219950   0.007080471
## key_confidence             0.01894425  0.027472060   0.033262222
## timbre_4_min               0.07040792  0.136746164   0.210352846
## timbre_10_min              0.16163316  0.043954487   0.064520244
## timbre_1_min               0.20827637  0.352448987   0.135958877
## tempo                      0.07772488  0.084994775   0.003122385
##                          timbre_8_max timbre_7_min timbre_0_max
## timbre_6_min              0.343360348  0.453379045 0.0696024349
## timbre_4_max              0.295179151  0.416885588 0.0136622340
## timbre_11_min             0.272093182  0.243882172 0.0271179570
## timbre_8_max              0.000000000  0.311466153 0.1395411785
## timbre_7_min              0.311466153  0.000000000 0.0378223542
## timbre_0_max              0.139541178  0.037822354 0.0000000000
## timbre_9_max              0.273580091  0.480306074 0.0481018049
## timbre_11_max             0.317062203  0.212158521 0.1192869292
## timbre_2_min              0.262841319  0.382043213 0.0402265624
## pitch                     0.294399713  0.336376154 0.0493222539
## timbre_5_min              0.195179188  0.264235231 0.0470870381
## timbre_6_max              0.220280830  0.230236051 0.0059826955
## energy                    0.282351251  0.329769434 0.5632351479
## timbre_1_max              0.147887015  0.319382009 0.0462136599
## timbre_10_max             0.108968439  0.172849789 0.0459537656
## timbre_8_min              0.102824593  0.337189672 0.0240473720
## timbre_2_max              0.154130354  0.147575396 0.0585797233
## timbre_9_min              0.247454393  0.480227550 0.0327041012
## tempo_confidence          0.004713273  0.051643291 0.0774819401
## timbre_5_max              0.096891888  0.224828057 0.0268005031
## timbre_0_min              0.001905078  0.042752032 0.0474829253
## timesignature_confidence  0.034307220  0.073592966 0.1676558113
## timesignature             0.014355589  0.048818652 0.1000202527
## timbre_7_max              0.157315943  0.396808582 0.0131923760
## timbre_3_max              0.016934165  0.077538391 0.1384427330
## timbre_3_min              0.114592983  0.312779539 0.0475486231
## key                       0.003711362  0.003365663 0.0154776480
## key_confidence            0.003680199  0.022219296 0.0606462420
## timbre_4_min              0.154246437  0.080236736 0.0429582944
## timbre_10_min             0.009578378  0.188496107 0.1625037970
## timbre_1_min              0.286368842  0.375513659 0.0982142401
## tempo                     0.018156497  0.089338029 0.0006267798
##                          timbre_9_max timbre_11_max timbre_2_min
## timbre_6_min              0.350398795   0.287627661   0.33468254
## timbre_4_max              0.299157415   0.269329755   0.28099799
## timbre_11_min             0.197884104   0.100340776   0.21284636
## timbre_8_max              0.273580091   0.317062203   0.26284132
## timbre_7_min              0.480306074   0.212158521   0.38204321
## timbre_0_max              0.048101805   0.119286929   0.04022656
## timbre_9_max              0.000000000   0.148938415   0.33962450
## timbre_11_max             0.148938415   0.000000000   0.18760710
## timbre_2_min              0.339624497   0.187607098   0.00000000
## pitch                     0.218041638   0.200903150   0.35066686
## timbre_5_min              0.243147818   0.124021662   0.42052948
## timbre_6_max              0.233402549   0.293563661   0.24058235
## energy                    0.152416686   0.243295160   0.24300973
## timbre_1_max              0.349430332   0.176178429   0.24591835
## timbre_10_max             0.280516892   0.109054150   0.22991416
## timbre_8_min              0.229929550   0.301381265   0.25895382
## timbre_2_max              0.134203698   0.204646489   0.06892429
## timbre_9_min              0.311198690   0.232306316   0.44355187
## tempo_confidence          0.080879806   0.039039039   0.13353078
## timbre_5_max              0.393712297   0.064908698   0.24964052
## timbre_0_min              0.010839078   0.061334471   0.13387939
## timesignature_confidence  0.068768307   0.031432484   0.11076993
## timesignature             0.035693641   0.002586617   0.03971574
## timbre_7_max              0.367655592   0.123413462   0.32996685
## timbre_3_max              0.151310307   0.002925378   0.29105273
## timbre_3_min              0.326593257   0.091720171   0.21467925
## key                       0.008604285   0.017008236   0.03457335
## key_confidence            0.024188984   0.032322936   0.01670375
## timbre_4_min              0.123143414   0.131563583   0.17753915
## timbre_10_min             0.259533956   0.009378114   0.28249002
## timbre_1_min              0.187687648   0.203375969   0.32539515
## tempo                     0.049463498   0.012129520   0.04339832
##                               pitch timbre_5_min timbre_6_max      energy
## timbre_6_min             0.27804672  0.230666903  0.109892063 0.294757232
## timbre_4_max             0.30210668  0.244348086  0.299433795 0.289538100
## timbre_11_min            0.26153897  0.205150079  0.283300815 0.066856448
## timbre_8_max             0.29439971  0.195179188  0.220280830 0.282351251
## timbre_7_min             0.33637615  0.264235231  0.230236051 0.329769434
## timbre_0_max             0.04932225  0.047087038  0.005982696 0.563235148
## timbre_9_max             0.21804164  0.243147818  0.233402549 0.152416686
## timbre_11_max            0.20090315  0.124021662  0.293563661 0.243295160
## timbre_2_min             0.35066686  0.420529476  0.240582350 0.243009734
## pitch                    0.00000000  0.223960194  0.219001377 0.312262657
## timbre_5_min             0.22396019  0.000000000  0.226504870 0.036369274
## timbre_6_max             0.21900138  0.226504870  0.000000000 0.098479283
## energy                   0.31226266  0.036369274  0.098479283 0.000000000
## timbre_1_max             0.09810809  0.191875077  0.166754561 0.002001210
## timbre_10_max            0.08725314  0.184676809  0.089273384 0.022486279
## timbre_8_min             0.22939066  0.218309873  0.356422030 0.182794170
## timbre_2_max             0.06848500  0.089580677  0.367687159 0.013518960
## timbre_9_min             0.28790551  0.416216275  0.248575771 0.317943097
## tempo_confidence         0.05232348  0.108568422  0.062042883 0.151835896
## timbre_5_max             0.10359583  0.144038997  0.156548395 0.041564526
## timbre_0_min             0.05818190  0.030193786  0.006021398 0.090120314
## timesignature_confidence 0.09687713  0.111815829  0.076600503 0.105931843
## timesignature            0.09444193  0.051730222  0.055446717 0.086373400
## timbre_7_max             0.18438616  0.259243709  0.195604792 0.172629495
## timbre_3_max             0.03334388  0.406337856  0.082302782 0.160216580
## timbre_3_min             0.13616712  0.092879786  0.186143204 0.153648190
## key                      0.01145964  0.023763318  0.032824721 0.006268243
## key_confidence           0.08485858  0.021182571  0.004471869 0.055377445
## timbre_4_min             0.10820600  0.138930408  0.181223606 0.039110291
## timbre_10_min            0.01886741  0.125766699  0.094548646 0.108898889
## timbre_1_min             0.39195512  0.185315298  0.200257312 0.484662808
## tempo                    0.03859802  0.008690528  0.017609126 0.154989603
##                          timbre_1_max timbre_10_max timbre_8_min
## timbre_6_min               0.30491475   0.174794261  0.280754431
## timbre_4_max               0.31622699   0.104252143  0.391282450
## timbre_11_min              0.16563865   0.105775522  0.288524969
## timbre_8_max               0.14788701   0.108968439  0.102824593
## timbre_7_min               0.31938201   0.172849789  0.337189672
## timbre_0_max               0.04621366   0.045953766  0.024047372
## timbre_9_max               0.34943033   0.280516892  0.229929550
## timbre_11_max              0.17617843   0.109054150  0.301381265
## timbre_2_min               0.24591835   0.229914156  0.258953817
## pitch                      0.09810809   0.087253145  0.229390664
## timbre_5_min               0.19187508   0.184676809  0.218309873
## timbre_6_max               0.16675456   0.089273384  0.356422030
## energy                     0.00200121   0.022486279  0.182794170
## timbre_1_max               0.00000000   0.145669916  0.311733156
## timbre_10_max              0.14566992   0.000000000  0.089497147
## timbre_8_min               0.31173316   0.089497147  0.000000000
## timbre_2_max               0.28261858   0.076449437  0.279084929
## timbre_9_min               0.20999204   0.168599300  0.354235603
## tempo_confidence           0.04626091   0.082235706  0.042807148
## timbre_5_max               0.15094184   0.342166572  0.087237336
## timbre_0_min               0.08527484   0.044939690  0.060985110
## timesignature_confidence   0.04757327   0.024220307  0.079158394
## timesignature              0.02304274   0.024450016  0.050334757
## timbre_7_max               0.22382410   0.190467297  0.229674385
## timbre_3_max               0.14297015   0.179598318  0.045053702
## timbre_3_min               0.09181731   0.071899263  0.104275059
## key                        0.00835810   0.012914541  0.019667315
## key_confidence             0.04455801   0.014574252  0.005621326
## timbre_4_min               0.15063941   0.105485194  0.231026499
## timbre_10_min              0.20307467   0.189783690  0.080554935
## timbre_1_min               0.05414452   0.020543933  0.219176282
## tempo                      0.04376397   0.007268116  0.071572973
##                          timbre_2_max timbre_9_min tempo_confidence
## timbre_6_min             0.2302414573   0.35086071      0.017860800
## timbre_4_max             0.3956877151   0.37746369      0.040958171
## timbre_11_min            0.2532807536   0.18977980      0.093264502
## timbre_8_max             0.1541303543   0.24745439      0.004713273
## timbre_7_min             0.1475753958   0.48022755      0.051643291
## timbre_0_max             0.0585797233   0.03270410      0.077481940
## timbre_9_max             0.1342036983   0.31119869      0.080879806
## timbre_11_max            0.2046464888   0.23230632      0.039039039
## timbre_2_min             0.0689242891   0.44355187      0.133530783
## pitch                    0.0684850040   0.28790551      0.052323478
## timbre_5_min             0.0895806765   0.41621628      0.108568422
## timbre_6_max             0.3676871592   0.24857577      0.062042883
## energy                   0.0135189596   0.31794310      0.151835896
## timbre_1_max             0.2826185842   0.20999204      0.046260907
## timbre_10_max            0.0764494369   0.16859930      0.082235706
## timbre_8_min             0.2790849287   0.35423560      0.042807148
## timbre_2_max             0.0000000000   0.11535359      0.032755301
## timbre_9_min             0.1153535855   0.00000000      0.032859147
## tempo_confidence         0.0327553005   0.03285915      0.000000000
## timbre_5_max             0.0379334991   0.19928533      0.086076636
## timbre_0_min             0.1071360022   0.10070341      0.031845980
## timesignature_confidence 0.0018110808   0.05111407      0.328598944
## timesignature            0.0197393819   0.01465327      0.163705257
## timbre_7_max             0.0948650045   0.43881220      0.035375496
## timbre_3_max             0.0698456281   0.26829132      0.032902517
## timbre_3_min             0.0733918083   0.18491055      0.013770331
## key                      0.0182015842   0.02025505      0.013342528
## key_confidence           0.0005591351   0.01834614      0.013629201
## timbre_4_min             0.0888438468   0.10583691      0.002233332
## timbre_10_min            0.0691323664   0.13245828      0.043191735
## timbre_1_min             0.0355838445   0.44636549      0.049810469
## tempo                    0.0136902058   0.07339777      0.023280660
##                          timbre_5_max timbre_0_min
## timbre_6_min              0.200404407  0.048253824
## timbre_4_max              0.109963011  0.044692613
## timbre_11_min             0.126617896  0.011006016
## timbre_8_max              0.096891888  0.001905078
## timbre_7_min              0.224828057  0.042752032
## timbre_0_max              0.026800503  0.047482925
## timbre_9_max              0.393712297  0.010839078
## timbre_11_max             0.064908698  0.061334471
## timbre_2_min              0.249640519  0.133879387
## pitch                     0.103595831  0.058181899
## timbre_5_min              0.144038997  0.030193786
## timbre_6_max              0.156548395  0.006021398
## energy                    0.041564526  0.090120314
## timbre_1_max              0.150941837  0.085274839
## timbre_10_max             0.342166572  0.044939690
## timbre_8_min              0.087237336  0.060985110
## timbre_2_max              0.037933499  0.107136002
## timbre_9_min              0.199285332  0.100703414
## tempo_confidence          0.086076636  0.031845980
## timbre_5_max              0.000000000  0.024819207
## timbre_0_min              0.024819207  0.000000000
## timesignature_confidence  0.050928400  0.026330389
## timesignature             0.031055225  0.013387463
## timbre_7_max              0.214537373  0.094520663
## timbre_3_max              0.194677953  0.082459551
## timbre_3_min              0.283879738  0.022874423
## key                       0.008153236  0.008790121
## key_confidence            0.020663866  0.044227848
## timbre_4_min              0.118015402  0.020810701
## timbre_10_min             0.301989938  0.041833097
## timbre_1_min              0.049218571  0.107282699
## tempo                     0.009726892  0.021308733
##                          timesignature_confidence timesignature
## timbre_6_min                          0.031361625   0.029081140
## timbre_4_max                          0.080582500   0.038111591
## timbre_11_min                         0.073911622   0.071138001
## timbre_8_max                          0.034307220   0.014355589
## timbre_7_min                          0.073592966   0.048818652
## timbre_0_max                          0.167655811   0.100020253
## timbre_9_max                          0.068768307   0.035693641
## timbre_11_max                         0.031432484   0.002586617
## timbre_2_min                          0.110769925   0.039715742
## pitch                                 0.096877126   0.094441934
## timbre_5_min                          0.111815829   0.051730222
## timbre_6_max                          0.076600503   0.055446717
## energy                                0.105931843   0.086373400
## timbre_1_max                          0.047573274   0.023042736
## timbre_10_max                         0.024220307   0.024450016
## timbre_8_min                          0.079158394   0.050334757
## timbre_2_max                          0.001811081   0.019739382
## timbre_9_min                          0.051114075   0.014653267
## tempo_confidence                      0.328598944   0.163705257
## timbre_5_max                          0.050928400   0.031055225
## timbre_0_min                          0.026330389   0.013387463
## timesignature_confidence              0.000000000   0.343982155
## timesignature                         0.343982155   0.000000000
## timbre_7_max                          0.033254180   0.002040133
## timbre_3_max                          0.004536818   0.022124539
## timbre_3_min                          0.022642567   0.006046027
## key                                   0.006794214   0.017572596
## key_confidence                        0.016430285   0.015918168
## timbre_4_min                          0.020021944   0.004021022
## timbre_10_min                         0.047834679   0.008729925
## timbre_1_min                          0.024430815   0.016925000
## tempo                                 0.029783675   0.048710910
##                          timbre_7_max timbre_3_max timbre_3_min
## timbre_6_min              0.296872849  0.153038250 0.1820907729
## timbre_4_max              0.243897425  0.009126047 0.1104430347
## timbre_11_min             0.121150310  0.020697075 0.0633110132
## timbre_8_max              0.157315943  0.016934165 0.1145929826
## timbre_7_min              0.396808582  0.077538391 0.3127795392
## timbre_0_max              0.013192376  0.138442733 0.0475486231
## timbre_9_max              0.367655592  0.151310307 0.3265932568
## timbre_11_max             0.123413462  0.002925378 0.0917201714
## timbre_2_min              0.329966846  0.291052730 0.2146792494
## pitch                     0.184386155  0.033343878 0.1361671151
## timbre_5_min              0.259243709  0.406337856 0.0928797862
## timbre_6_max              0.195604792  0.082302782 0.1861432039
## energy                    0.172629495  0.160216580 0.1536481903
## timbre_1_max              0.223824104  0.142970150 0.0918173123
## timbre_10_max             0.190467297  0.179598318 0.0718992627
## timbre_8_min              0.229674385  0.045053702 0.1042750586
## timbre_2_max              0.094865005  0.069845628 0.0733918083
## timbre_9_min              0.438812202  0.268291318 0.1849105474
## tempo_confidence          0.035375496  0.032902517 0.0137703313
## timbre_5_max              0.214537373  0.194677953 0.2838797383
## timbre_0_min              0.094520663  0.082459551 0.0228744234
## timesignature_confidence  0.033254180  0.004536818 0.0226425671
## timesignature             0.002040133  0.022124539 0.0060460266
## timbre_7_max              0.000000000  0.340934695 0.2000254906
## timbre_3_max              0.340934695  0.000000000 0.1323063227
## timbre_3_min              0.200025491  0.132306323 0.0000000000
## key                       0.005510934  0.010024050 0.0080427657
## key_confidence            0.003676716  0.049084134 0.0003020344
## timbre_4_min              0.084025763  0.106272159 0.0599235058
## timbre_10_min             0.248894049  0.509846782 0.2514500287
## timbre_1_min              0.275511097  0.109976920 0.1324010792
## tempo                     0.075725817  0.005910305 0.0479623596
##                                  key key_confidence timbre_4_min
## timbre_6_min             0.017555376   0.0189442544  0.070407925
## timbre_4_max             0.005219950   0.0274720602  0.136746164
## timbre_11_min            0.007080471   0.0332622220  0.210352846
## timbre_8_max             0.003711362   0.0036801985  0.154246437
## timbre_7_min             0.003365663   0.0222192962  0.080236736
## timbre_0_max             0.015477648   0.0606462420  0.042958294
## timbre_9_max             0.008604285   0.0241889840  0.123143414
## timbre_11_max            0.017008236   0.0323229361  0.131563583
## timbre_2_min             0.034573353   0.0167037493  0.177539148
## pitch                    0.011459639   0.0848585775  0.108205998
## timbre_5_min             0.023763318   0.0211825706  0.138930408
## timbre_6_max             0.032824721   0.0044718691  0.181223606
## energy                   0.006268243   0.0553774453  0.039110291
## timbre_1_max             0.008358100   0.0445580080  0.150639406
## timbre_10_max            0.012914541   0.0145742522  0.105485194
## timbre_8_min             0.019667315   0.0056213263  0.231026499
## timbre_2_max             0.018201584   0.0005591351  0.088843847
## timbre_9_min             0.020255052   0.0183461407  0.105836913
## tempo_confidence         0.013342528   0.0136292011  0.002233332
## timbre_5_max             0.008153236   0.0206638661  0.118015402
## timbre_0_min             0.008790121   0.0442278483  0.020810701
## timesignature_confidence 0.006794214   0.0164302851  0.020021944
## timesignature            0.017572596   0.0159181676  0.004021022
## timbre_7_max             0.005510934   0.0036767165  0.084025763
## timbre_3_max             0.010024050   0.0490841335  0.106272159
## timbre_3_min             0.008042766   0.0003020344  0.059923506
## key                      0.000000000   0.0435718053  0.027567496
## key_confidence           0.043571805   0.0000000000  0.024946146
## timbre_4_min             0.027567496   0.0249461462  0.000000000
## timbre_10_min            0.007024254   0.0604936404  0.114621536
## timbre_1_min             0.010649185   0.0757591439  0.026381988
## tempo                    0.018874310   0.0709959251  0.018817596
##                          timbre_10_min timbre_1_min        tempo
## timbre_6_min               0.161633164   0.20827637 0.0777248783
## timbre_4_max               0.043954487   0.35244899 0.0849947746
## timbre_11_min              0.064520244   0.13595888 0.0031223853
## timbre_8_max               0.009578378   0.28636884 0.0181564969
## timbre_7_min               0.188496107   0.37551366 0.0893380293
## timbre_0_max               0.162503797   0.09821424 0.0006267798
## timbre_9_max               0.259533956   0.18768765 0.0494634981
## timbre_11_max              0.009378114   0.20337597 0.0121295203
## timbre_2_min               0.282490023   0.32539515 0.0433983220
## pitch                      0.018867411   0.39195512 0.0385980153
## timbre_5_min               0.125766699   0.18531530 0.0086905285
## timbre_6_max               0.094548646   0.20025731 0.0176091258
## energy                     0.108898889   0.48466281 0.1549896030
## timbre_1_max               0.203074667   0.05414452 0.0437639708
## timbre_10_max              0.189783690   0.02054393 0.0072681162
## timbre_8_min               0.080554935   0.21917628 0.0715729733
## timbre_2_max               0.069132366   0.03558384 0.0136902058
## timbre_9_min               0.132458282   0.44636549 0.0733977686
## tempo_confidence           0.043191735   0.04981047 0.0232806600
## timbre_5_max               0.301989938   0.04921857 0.0097268920
## timbre_0_min               0.041833097   0.10728270 0.0213087331
## timesignature_confidence   0.047834679   0.02443081 0.0297836750
## timesignature              0.008729925   0.01692500 0.0487109099
## timbre_7_max               0.248894049   0.27551110 0.0757258169
## timbre_3_max               0.509846782   0.10997692 0.0059103046
## timbre_3_min               0.251450029   0.13240108 0.0479623596
## key                        0.007024254   0.01064919 0.0188743099
## key_confidence             0.060493640   0.07575914 0.0709959251
## timbre_4_min               0.114621536   0.02638199 0.0188175962
## timbre_10_min              0.000000000   0.05979506 0.0313747819
## timbre_1_min               0.059795059   0.00000000 0.0794040880
## tempo                      0.031374782   0.07940409 0.0000000000
##                          id        cor.y   cor.y.abs cor.low
## 20             timbre_4_max  0.197034666 0.197034666       1
## 28             timbre_8_max  0.162722687 0.162722687       1
## 30             timbre_9_max  0.145756048 0.145756048       1
## 14            timbre_11_max  0.143718363 0.143718363       1
## 24             timbre_6_max  0.123432076 0.123432076       1
## 10             timbre_1_max  0.106338252 0.106338252       1
## 12            timbre_10_max  0.104441775 0.104441775       1
## 16             timbre_2_max  0.096545551 0.096545551       1
## 7          tempo_confidence  0.086315020 0.086315020       1
## 22             timbre_5_max  0.076923330 0.076923330       1
## 9              timbre_0_min  0.068825164 0.068825164       1
## 33 timesignature_confidence  0.063023674 0.063023674       1
## 32            timesignature  0.046630910 0.046630910       1
## 26             timbre_7_max  0.045966630 0.045966630       1
## 2                       key  0.025326063 0.025326063       1
## 3            key_confidence  0.010021403 0.010021403       1
## 13            timbre_10_min  0.005257590 0.005257590       1
## 11             timbre_1_min  0.003659089 0.003659089       1
## 6                     tempo -0.003577709 0.003577709       1
## 21             timbre_4_min -0.006010064 0.006010064       1
## 19             timbre_3_min -0.026442066 0.026442066       1
## 18             timbre_3_max -0.026744876 0.026744876       1
## 4                  loudness -0.086546031 0.086546031      NA
## 31             timbre_9_min -0.088167503 0.088167503       1
## 29             timbre_8_min -0.101131084 0.101131084       1
## 1                    energy -0.120897670 0.120897670       1
## 23             timbre_5_min -0.123590779 0.123590779       1
## 5                     pitch -0.135723510 0.135723510       1
## 17             timbre_2_min -0.136085935 0.136085935       1
## 8              timbre_0_max -0.152171355 0.152171355       1
## 27             timbre_7_min -0.155645339 0.155645339       1
## 15            timbre_11_min -0.193049686 0.193049686       1
## 25             timbre_6_min -0.216378268 0.216378268       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5643  -0.5643  -0.5643  -0.5643   1.9575  
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.75672    0.03326  -52.82   <2e-16 ***
## .rnorm            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 6017.5  on 7200  degrees of freedom
## AIC: 6019.5
## 
## Number of Fisher Scoring iterations: 4
## 
##    feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 1 .rnorm  7201       NA       NA           NA 57363.21      NA 6019.507
##   auc.fit auc.OOB
## 1     0.5     0.5
```

```r
glb_dmy_mdl <- glb_mdl

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4435  -0.6018  -0.4928  -0.3770   2.4062  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -3.641821   0.126663  -28.75   <2e-16 ***
## timbre_4_max  0.016480   0.001014   16.25   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 5742.3  on 7199  degrees of freedom
## AIC: 5746.3
## 
## Number of Fisher Scoring iterations: 5
## 
##          feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB
## 2 timbre_4_max  7201       NA       NA           NA 60834.18      NA
## 1       .rnorm  7201       NA       NA           NA 57363.21      NA
##    AIC.fit   auc.fit   auc.OOB
## 2 5746.340 0.6644847 0.6446076
## 1 6019.507 0.5000000 0.5000000
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9846  -0.5998  -0.4890  -0.3759   2.3718  
## 
## Coefficients:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -3.620e+00  1.271e-01 -28.480  < 2e-16 ***
## timbre_4_max           1.426e-02  1.187e-03  12.020  < 2e-16 ***
## timbre_4_max:loudness -2.199e-04  6.026e-05  -3.648 0.000264 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 5729.2  on 7198  degrees of freedom
## AIC: 5735.2
## 
## Number of Fisher Scoring iterations: 5
## 
##                                 feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit
## 3 timbre_4_max, timbre_4_max:loudness  7201       NA       NA           NA
## 2                        timbre_4_max  7201       NA       NA           NA
## 1                              .rnorm  7201       NA       NA           NA
##    SSE.fit SSE.OOB  AIC.fit   auc.fit   auc.OOB
## 3 59410.05      NA 5735.247 0.6722422 0.6591277
## 2 60834.18      NA 5746.340 0.6644847 0.6446076
## 1 57363.21      NA 6019.507 0.5000000 0.5000000
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0983  -0.5607  -0.3602  -0.1902   3.3107  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.241e+00  7.465e-01  -3.002 0.002686 ** 
## timbre_4_max              6.306e-03  1.532e-03   4.115 3.87e-05 ***
## timbre_8_max              6.423e-03  2.950e-03   2.177 0.029497 *  
## timbre_9_max              3.525e-03  2.377e-03   1.483 0.138017    
## timbre_11_max             1.829e-02  3.341e-03   5.476 4.34e-08 ***
## timbre_6_max              3.814e-03  2.157e-03   1.768 0.076982 .  
## timbre_1_max             -7.830e-04  7.064e-04  -1.108 0.267650    
## timbre_10_max             7.367e-03  1.731e-03   4.255 2.09e-05 ***
## timbre_2_max              3.889e-04  8.964e-04   0.434 0.664427    
## tempo_confidence          5.497e-01  1.407e-01   3.906 9.40e-05 ***
## timbre_5_max              6.937e-04  7.807e-04   0.889 0.374256    
## timbre_0_min              2.479e-02  4.240e-03   5.847 5.01e-09 ***
## timesignature_confidence  6.885e-01  1.924e-01   3.578 0.000346 ***
## timesignature             1.625e-01  8.734e-02   1.860 0.062873 .  
## timbre_7_max             -3.158e-03  1.811e-03  -1.744 0.081090 .  
## key                       1.740e-02  1.026e-02   1.697 0.089740 .  
## key_confidence            2.954e-01  1.394e-01   2.118 0.034163 *  
## timbre_10_min             2.993e-03  1.804e-03   1.660 0.097004 .  
## timbre_1_min              7.143e-03  7.710e-04   9.265  < 2e-16 ***
## tempo                     5.521e-04  1.665e-03   0.332 0.740226    
## timbre_4_min              9.115e-03  1.952e-03   4.670 3.02e-06 ***
## timbre_3_min              6.500e-04  5.949e-04   1.093 0.274524    
## timbre_3_max             -2.462e-03  5.674e-04  -4.339 1.43e-05 ***
## timbre_9_min             -4.282e-04  2.955e-03  -0.145 0.884792    
## timbre_8_min              4.488e-03  2.810e-03   1.597 0.110254    
## energy                    1.813e-01  2.608e-01   0.695 0.486991    
## timbre_5_min             -5.641e-03  1.255e-03  -4.495 6.95e-06 ***
## pitch                    -5.150e+01  6.857e+00  -7.511 5.87e-14 ***
## timbre_2_min             -1.579e-03  1.109e-03  -1.424 0.154531    
## timbre_0_max             -1.007e-01  1.178e-02  -8.551  < 2e-16 ***
## timbre_7_min             -5.102e-03  1.755e-03  -2.907 0.003644 ** 
## timbre_11_min            -2.837e-02  3.630e-03  -7.815 5.48e-15 ***
## timbre_6_min             -1.612e-02  2.235e-03  -7.214 5.45e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4871.8  on 7168  degrees of freedom
## AIC: 4937.8
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## 4 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## 3                                                                                                                                                                                                                                                                                                                                                                                                                      timbre_4_max, timbre_4_max:loudness
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                             timbre_4_max
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 4  7201       NA       NA           NA 172410.48      NA 4937.849
## 3  7201       NA       NA           NA  59410.05      NA 5735.247
## 2  7201       NA       NA           NA  60834.18      NA 5746.340
## 1  7201       NA       NA           NA  57363.21      NA 6019.507
##     auc.fit   auc.OOB
## 4 0.8003625 0.8430854
## 3 0.6722422 0.6591277
## 2 0.6644847 0.6446076
## 1 0.5000000 0.5000000
```

```r
glb_sel_mdl <- glb_mdl

# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9220  -0.5399  -0.3459  -0.1845   3.0770  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.470e+01  1.806e+00   8.138 4.03e-16 ***
## timesignature             1.264e-01  8.674e-02   1.457 0.145050    
## timesignature_confidence  7.450e-01  1.953e-01   3.815 0.000136 ***
## loudness                  2.999e-01  2.917e-02  10.282  < 2e-16 ***
## tempo                     3.634e-04  1.691e-03   0.215 0.829889    
## tempo_confidence          4.732e-01  1.422e-01   3.329 0.000873 ***
## key                       1.588e-02  1.039e-02   1.529 0.126349    
## key_confidence            3.087e-01  1.412e-01   2.187 0.028760 *  
## energy                   -1.502e+00  3.099e-01  -4.847 1.25e-06 ***
## pitch                    -4.491e+01  6.835e+00  -6.570 5.02e-11 ***
## timbre_0_min              2.316e-02  4.256e-03   5.441 5.29e-08 ***
## timbre_0_max             -3.310e-01  2.569e-02 -12.882  < 2e-16 ***
## timbre_1_min              5.881e-03  7.798e-04   7.542 4.64e-14 ***
## timbre_1_max             -2.449e-04  7.152e-04  -0.342 0.732087    
## timbre_2_min             -2.127e-03  1.126e-03  -1.889 0.058843 .  
## timbre_2_max              6.586e-04  9.066e-04   0.726 0.467571    
## timbre_3_min              6.920e-04  5.985e-04   1.156 0.247583    
## timbre_3_max             -2.967e-03  5.815e-04  -5.103 3.34e-07 ***
## timbre_4_min              1.040e-02  1.985e-03   5.237 1.63e-07 ***
## timbre_4_max              6.110e-03  1.550e-03   3.942 8.10e-05 ***
## timbre_5_min             -5.598e-03  1.277e-03  -4.385 1.16e-05 ***
## timbre_5_max              7.736e-05  7.935e-04   0.097 0.922337    
## timbre_6_min             -1.686e-02  2.264e-03  -7.445 9.66e-14 ***
## timbre_6_max              3.668e-03  2.190e-03   1.675 0.093875 .  
## timbre_7_min             -4.549e-03  1.781e-03  -2.554 0.010661 *  
## timbre_7_max             -3.774e-03  1.832e-03  -2.060 0.039408 *  
## timbre_8_min              3.911e-03  2.851e-03   1.372 0.170123    
## timbre_8_max              4.011e-03  3.003e-03   1.336 0.181620    
## timbre_9_min              1.367e-03  2.998e-03   0.456 0.648356    
## timbre_9_max              1.603e-03  2.434e-03   0.659 0.510188    
## timbre_10_min             4.126e-03  1.839e-03   2.244 0.024852 *  
## timbre_10_max             5.825e-03  1.769e-03   3.292 0.000995 ***
## timbre_11_min            -2.625e-02  3.693e-03  -7.108 1.18e-12 ***
## timbre_11_max             1.967e-02  3.385e-03   5.811 6.21e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4759.2  on 7167  degrees of freedom
## AIC: 4827.2
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 4           timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## 5 timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                timbre_4_max, timbre_4_max:loudness
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                       timbre_4_max
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 4  7201       NA       NA           NA 172410.48      NA 4937.849
## 5  7201       NA       NA           NA 121413.77      NA 4827.154
## 3  7201       NA       NA           NA  59410.05      NA 5735.247
## 2  7201       NA       NA           NA  60834.18      NA 5746.340
## 1  7201       NA       NA           NA  57363.21      NA 6019.507
##     auc.fit   auc.OOB
## 4 0.8003625 0.8430854
## 5 0.8138747 0.8425996
## 3 0.6722422 0.6591277
## 2 0.6644847 0.6446076
## 1 0.5000000 0.5000000
```

```r
glb_mdl1 <- glb_mdl

# User specified
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(union(glb_predct_var, glb_exclude_vars_as_features), c("loudness"))),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0983  -0.5607  -0.3602  -0.1902   3.3107  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.241e+00  7.465e-01  -3.002 0.002686 ** 
## timesignature             1.625e-01  8.734e-02   1.860 0.062873 .  
## timesignature_confidence  6.885e-01  1.924e-01   3.578 0.000346 ***
## tempo                     5.521e-04  1.665e-03   0.332 0.740226    
## tempo_confidence          5.497e-01  1.407e-01   3.906 9.40e-05 ***
## key                       1.740e-02  1.026e-02   1.697 0.089740 .  
## key_confidence            2.954e-01  1.394e-01   2.118 0.034163 *  
## energy                    1.813e-01  2.608e-01   0.695 0.486991    
## pitch                    -5.150e+01  6.857e+00  -7.511 5.87e-14 ***
## timbre_0_min              2.479e-02  4.240e-03   5.847 5.01e-09 ***
## timbre_0_max             -1.007e-01  1.178e-02  -8.551  < 2e-16 ***
## timbre_1_min              7.143e-03  7.710e-04   9.265  < 2e-16 ***
## timbre_1_max             -7.830e-04  7.064e-04  -1.108 0.267650    
## timbre_2_min             -1.579e-03  1.109e-03  -1.424 0.154531    
## timbre_2_max              3.889e-04  8.964e-04   0.434 0.664427    
## timbre_3_min              6.500e-04  5.949e-04   1.093 0.274524    
## timbre_3_max             -2.462e-03  5.674e-04  -4.339 1.43e-05 ***
## timbre_4_min              9.115e-03  1.952e-03   4.670 3.02e-06 ***
## timbre_4_max              6.306e-03  1.532e-03   4.115 3.87e-05 ***
## timbre_5_min             -5.641e-03  1.255e-03  -4.495 6.95e-06 ***
## timbre_5_max              6.937e-04  7.807e-04   0.889 0.374256    
## timbre_6_min             -1.612e-02  2.235e-03  -7.214 5.45e-13 ***
## timbre_6_max              3.814e-03  2.157e-03   1.768 0.076982 .  
## timbre_7_min             -5.102e-03  1.755e-03  -2.907 0.003644 ** 
## timbre_7_max             -3.158e-03  1.811e-03  -1.744 0.081090 .  
## timbre_8_min              4.488e-03  2.810e-03   1.597 0.110254    
## timbre_8_max              6.423e-03  2.950e-03   2.177 0.029497 *  
## timbre_9_min             -4.282e-04  2.955e-03  -0.145 0.884792    
## timbre_9_max              3.525e-03  2.377e-03   1.483 0.138017    
## timbre_10_min             2.993e-03  1.804e-03   1.660 0.097004 .  
## timbre_10_max             7.367e-03  1.731e-03   4.255 2.09e-05 ***
## timbre_11_min            -2.837e-02  3.630e-03  -7.815 5.48e-15 ***
## timbre_11_max             1.829e-02  3.341e-03   5.476 4.34e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4871.8  on 7168  degrees of freedom
## AIC: 4937.8
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 4           timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## 6           timesignature, timesignature_confidence, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 5 timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                timbre_4_max, timbre_4_max:loudness
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                       timbre_4_max
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 4  7201       NA       NA           NA 172410.48      NA 4937.849
## 6  7201       NA       NA           NA 172410.48      NA 4937.849
## 5  7201       NA       NA           NA 121413.77      NA 4827.154
## 3  7201       NA       NA           NA  59410.05      NA 5735.247
## 2  7201       NA       NA           NA  60834.18      NA 5746.340
## 1  7201       NA       NA           NA  57363.21      NA 6019.507
##     auc.fit   auc.OOB
## 4 0.8003625 0.8430854
## 6 0.8003625 0.8430854
## 5 0.8138747 0.8425996
## 3 0.6722422 0.6591277
## 2 0.6644847 0.6446076
## 1 0.5000000 0.5000000
```

```r
glb_mdl2 <- glb_mdl

ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(union(glb_predct_var, glb_exclude_vars_as_features), c("energy"))),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
## timesignature             1.151e-01  8.726e-02   1.319 0.187183    
## timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
## loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
## tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
## tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
## key                       1.649e-02  1.035e-02   1.593 0.111056    
## key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
## pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
## timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
## timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
## timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
## timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
## timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
## timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
## timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
## timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
## timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
## timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
## timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
## timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
## timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
## timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
## timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
## timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
## timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
## timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
## timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
## timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
## timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
## timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
## timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
## timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4782.7  on 7168  degrees of freedom
## AIC: 4848.7
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 7         timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 4           timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## 6           timesignature, timesignature_confidence, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 5 timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                timbre_4_max, timbre_4_max:loudness
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                       timbre_4_max
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 7  7201       NA       NA           NA 230752.29      NA 4848.719
## 4  7201       NA       NA           NA 172410.48      NA 4937.849
## 6  7201       NA       NA           NA 172410.48      NA 4937.849
## 5  7201       NA       NA           NA 121413.77      NA 4827.154
## 3  7201       NA       NA           NA  59410.05      NA 5735.247
## 2  7201       NA       NA           NA  60834.18      NA 5746.340
## 1  7201       NA       NA           NA  57363.21      NA 6019.507
##     auc.fit   auc.OOB
## 7 0.8115728 0.8489690
## 4 0.8003625 0.8430854
## 6 0.8003625 0.8430854
## 5 0.8138747 0.8425996
## 3 0.6722422 0.6591277
## 2 0.6644847 0.6446076
## 1 0.5000000 0.5000000
```

```r
glb_sel_mdl <- glb_mdl3 <- glb_mdl

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression)
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))

if (glb_is_classification) {
    # Lower AIC is better
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

![](songs_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(glb_sel_mdl, glb_entity_df))
```

```
##                               Estimate   Std. Error      z value
## timbre_0_max             -3.104800e-01 0.0253654374 -12.24027957
## loudness                  2.305565e-01 0.0252798280   9.12017746
## timbre_6_min             -1.784468e-02 0.0022460477  -7.94492617
## pitch                    -5.328406e+01 6.7328543658  -7.91403684
## timbre_11_min            -2.637666e-02 0.0036829221  -7.16188327
## timbre_1_min              5.415971e-03 0.0007642681   7.08648035
## timbre_11_max             1.983605e-02 0.0033645987   5.89551747
## timbre_4_min              1.104648e-02 0.0019779298   5.58486958
## timbre_0_min              2.204524e-02 0.0042394195   5.20006053
## timbre_3_max             -2.963693e-03 0.0005757606  -5.14744054
## timbre_4_max              6.466786e-03 0.0015413225   4.19560865
## timbre_5_min             -5.134534e-03 0.0012689707  -4.04621978
## timesignature_confidence  7.142698e-01 0.1946175126   3.67012069
## timbre_10_max             5.792522e-03 0.0017585801   3.29386322
## timbre_7_min             -5.128431e-03 0.0017684788  -2.89991111
## tempo_confidence          3.840930e-01 0.1398349926   2.74675878
## key_confidence            3.394064e-01 0.1408743797   2.40928396
## timbre_10_min             4.050011e-03 0.0018269696   2.21679177
## timbre_2_min             -2.254350e-03 0.0011202944  -2.01228374
## timbre_7_max             -3.393508e-03 0.0018197583  -1.86481250
## key                       1.649459e-02 0.0103513882   1.59346608
## timbre_6_max              3.446871e-03 0.0021821447   1.57957961
## timbre_8_max              4.657803e-03 0.0029878965   1.55889026
## timesignature             1.150942e-01 0.0872615471   1.31895723
## timbre_8_min              3.686085e-03 0.0028330850   1.30108536
## timbre_1_max             -5.114625e-04 0.0007110114  -0.71934511
## timbre_9_max              1.341711e-03 0.0024239144   0.55353084
## timbre_3_min              3.178585e-04 0.0005868707   0.54161587
## timbre_2_max              4.118902e-04 0.0009019609   0.45666086
## tempo                    -6.459963e-04 0.0016654689  -0.38787654
## timbre_5_max              2.978963e-04 0.0007855547   0.37921774
## timbre_9_min             -9.318324e-05 0.0029568685  -0.03151416
##                                  Pr.z                       id fit.feat
## timbre_0_max             1.893792e-34             timbre_0_max     TRUE
## loudness                 7.500142e-20                 loudness     TRUE
## timbre_6_min             1.943065e-15             timbre_6_min     TRUE
## pitch                    2.491745e-15                    pitch     TRUE
## timbre_11_min            7.957617e-13            timbre_11_min     TRUE
## timbre_1_min             1.375657e-12             timbre_1_min     TRUE
## timbre_11_max            3.735090e-09            timbre_11_max     TRUE
## timbre_4_min             2.338757e-08             timbre_4_min     TRUE
## timbre_0_min             1.992236e-07             timbre_0_min     TRUE
## timbre_3_max             2.640647e-07             timbre_3_max     TRUE
## timbre_4_max             2.721398e-05             timbre_4_max     TRUE
## timbre_5_min             5.205137e-05             timbre_5_min     TRUE
## timesignature_confidence 2.424360e-04 timesignature_confidence     TRUE
## timbre_10_max            9.882057e-04            timbre_10_max     TRUE
## timbre_7_min             3.732685e-03             timbre_7_min     TRUE
## tempo_confidence         6.018738e-03         tempo_confidence     TRUE
## key_confidence           1.598386e-02           key_confidence     TRUE
## timbre_10_min            2.663732e-02            timbre_10_min     TRUE
## timbre_2_min             4.419004e-02             timbre_2_min     TRUE
## timbre_7_max             6.220769e-02             timbre_7_max     TRUE
## key                      1.110557e-01                      key     TRUE
## timbre_6_max             1.142032e-01             timbre_6_max     TRUE
## timbre_8_max             1.190224e-01             timbre_8_max     TRUE
## timesignature            1.871834e-01            timesignature     TRUE
## timbre_8_min             1.932292e-01             timbre_8_min     TRUE
## timbre_1_max             4.719283e-01             timbre_1_max     TRUE
## timbre_9_max             5.799000e-01             timbre_9_max     TRUE
## timbre_3_min             5.880832e-01             timbre_3_min     TRUE
## timbre_2_max             6.479148e-01             timbre_2_max     TRUE
## tempo                    6.981074e-01                    tempo     TRUE
## timbre_5_max             7.045262e-01             timbre_5_max     TRUE
## timbre_9_min             9.748595e-01             timbre_9_min     TRUE
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    # 0 & 1 does not generate outcomes for certain categories
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_clf_proba_threshold <- 0.45
    print(sprintf("Classifier Probability Threshold: %0.4f to sync w/ HW",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
## timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
## loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
## timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
## pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
## timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
## timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
## timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
## timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
## timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
## timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
## timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
## timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
## timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
## timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
## timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
## tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
## key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
## timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
## timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
## timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
## key                       1.649e-02  1.035e-02   1.593 0.111056    
## timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
## timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
## timesignature             1.151e-01  8.726e-02   1.319 0.187183    
## timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
## timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
## timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
## timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
## timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
## tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
## timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
## timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4782.7  on 7168  degrees of freedom
## AIC: 4848.7
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 7         timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 4           timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## 6           timesignature, timesignature_confidence, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 5 timesignature, timesignature_confidence, loudness, tempo, tempo_confidence, key, key_confidence, energy, pitch, timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, timbre_2_min, timbre_2_max, timbre_3_min, timbre_3_max, timbre_4_min, timbre_4_max, timbre_5_min, timbre_5_max, timbre_6_min, timbre_6_max, timbre_7_min, timbre_7_max, timbre_8_min, timbre_8_max, timbre_9_min, timbre_9_max, timbre_10_min, timbre_10_max, timbre_11_min, timbre_11_max
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                timbre_4_max, timbre_4_max:loudness
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                       timbre_4_max
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## 8         timbre_0_max, loudness, timbre_6_min, pitch, timbre_11_min, timbre_1_min, timbre_11_max, timbre_4_min, timbre_0_min, timbre_3_max, timbre_4_max, timbre_5_min, timesignature_confidence, timbre_10_max, timbre_7_min, tempo_confidence, key_confidence, timbre_10_min, timbre_2_min, timbre_7_max, key, timbre_6_max, timbre_8_max, timesignature, timbre_8_min, timbre_1_max, timbre_9_max, timbre_3_min, timbre_2_max, tempo, timbre_5_max, timbre_9_min
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 7  7201       NA       NA           NA 230752.29      NA 4848.719
## 4  7201       NA       NA           NA 172410.48      NA 4937.849
## 6  7201       NA       NA           NA 172410.48      NA 4937.849
## 5  7201       NA       NA           NA 121413.77      NA 4827.154
## 3  7201       NA       NA           NA  59410.05      NA 5735.247
## 2  7201       NA       NA           NA  60834.18      NA 5746.340
## 1  7201       NA       NA           NA  57363.21      NA 6019.507
## 8  7201       NA       NA           NA 230752.29      NA 4848.719
##     auc.fit   auc.OOB
## 7 0.8115728 0.8489690
## 4 0.8003625 0.8430854
## 6 0.8003625 0.8430854
## 5 0.8138747 0.8425996
## 3 0.6722422 0.6591277
## 2 0.6644847 0.6446076
## 1 0.5000000 0.5000000
## 8 0.8115728        NA
```

![](songs_files/figure-html/fit_training.all-1.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.40791269
## 3        0.2 0.45992300
## 4        0.3 0.46445498
## 5        0.4 0.38008500
## 6        0.5 0.28034682
## 7        0.6 0.18800648
## 8        0.7 0.12000000
## 9        0.8 0.04044118
## 10       0.9 0.01312090
## 11       1.0 0.00000000
```

![](songs_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
## [1] "Classifier Probability Threshold: 0.4500 to sync w/ HW"
##   Top10 Top10.predict.0 Top10.predict.1
## 1     0            5953             188
## 2     1             812             248
## [1] "f.score=0.3316"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                          id        cor.y   cor.y.abs cor.low         Pr.z
## 8              timbre_0_max -0.152171355 0.152171355       1 1.893792e-34
## 4                  loudness -0.086546031 0.086546031      NA 7.500142e-20
## 25             timbre_6_min -0.216378268 0.216378268       1 1.943065e-15
## 5                     pitch -0.135723510 0.135723510       1 2.491745e-15
## 15            timbre_11_min -0.193049686 0.193049686       1 7.957617e-13
## 11             timbre_1_min  0.003659089 0.003659089       1 1.375657e-12
## 14            timbre_11_max  0.143718363 0.143718363       1 3.735090e-09
## 21             timbre_4_min -0.006010064 0.006010064       1 2.338757e-08
## 9              timbre_0_min  0.068825164 0.068825164       1 1.992236e-07
## 18             timbre_3_max -0.026744876 0.026744876       1 2.640647e-07
## 20             timbre_4_max  0.197034666 0.197034666       1 2.721398e-05
## 23             timbre_5_min -0.123590779 0.123590779       1 5.205137e-05
## 33 timesignature_confidence  0.063023674 0.063023674       1 2.424360e-04
## 12            timbre_10_max  0.104441775 0.104441775       1 9.882057e-04
## 27             timbre_7_min -0.155645339 0.155645339       1 3.732685e-03
## 7          tempo_confidence  0.086315020 0.086315020       1 6.018738e-03
## 3            key_confidence  0.010021403 0.010021403       1 1.598386e-02
## 13            timbre_10_min  0.005257590 0.005257590       1 2.663732e-02
## 17             timbre_2_min -0.136085935 0.136085935       1 4.419004e-02
## 26             timbre_7_max  0.045966630 0.045966630       1 6.220769e-02
## 2                       key  0.025326063 0.025326063       1 1.110557e-01
## 24             timbre_6_max  0.123432076 0.123432076       1 1.142032e-01
## 28             timbre_8_max  0.162722687 0.162722687       1 1.190224e-01
## 32            timesignature  0.046630910 0.046630910       1 1.871834e-01
## 29             timbre_8_min -0.101131084 0.101131084       1 1.932292e-01
## 10             timbre_1_max  0.106338252 0.106338252       1 4.719283e-01
## 30             timbre_9_max  0.145756048 0.145756048       1 5.799000e-01
## 19             timbre_3_min -0.026442066 0.026442066       1 5.880832e-01
## 16             timbre_2_max  0.096545551 0.096545551       1 6.479148e-01
## 6                     tempo -0.003577709 0.003577709       1 6.981074e-01
## 22             timbre_5_max  0.076923330 0.076923330       1 7.045262e-01
## 31             timbre_9_min -0.088167503 0.088167503       1 9.748595e-01
## 1                    energy -0.120897670 0.120897670       1           NA
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)) == 0)
            warning("No coefficients in selected model are statistically significant")
        else print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name, glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](songs_files/figure-html/fit_training.all-3.png) ![](songs_files/figure-html/fit_training.all-4.png) ![](songs_files/figure-html/fit_training.all-5.png) ![](songs_files/figure-html/fit_training.all-6.png) ![](songs_files/figure-html/fit_training.all-7.png) ![](songs_files/figure-html/fit_training.all-8.png) ![](songs_files/figure-html/fit_training.all-9.png) ![](songs_files/figure-html/fit_training.all-10.png) ![](songs_files/figure-html/fit_training.all-11.png) ![](songs_files/figure-html/fit_training.all-12.png) ![](songs_files/figure-html/fit_training.all-13.png) ![](songs_files/figure-html/fit_training.all-14.png) ![](songs_files/figure-html/fit_training.all-15.png) ![](songs_files/figure-html/fit_training.all-16.png) ![](songs_files/figure-html/fit_training.all-17.png) ![](songs_files/figure-html/fit_training.all-18.png) ![](songs_files/figure-html/fit_training.all-19.png) ![](songs_files/figure-html/fit_training.all-20.png) ![](songs_files/figure-html/fit_training.all-21.png) ![](songs_files/figure-html/fit_training.all-22.png) 

```
##     year       songtitle       artistname             songID
## 395 2009    Sweet Dreams          Beyonce SOXFARA13B1C88A5B3
## 397 2009 If I Were A Boy          Beyonce SONEARZ13A99C14D6C
## 398 2009            Halo          Beyonce SOFSRHH13775668108
## 400 2009       Womanizer   Britney Spears SOWDJOJ1373114BD21
## 401 2009   I Told You So Carrie Underwood SOELKUA135BB837641
## 412 2009  Love Sex Magic            Ciara SOMHVPR135C33B4BCD
##               artistID timesignature timesignature_confidence loudness
## 395 AR65K7A1187FB4DAA4             4                    1.000  -17.929
## 397 AR65K7A1187FB4DAA4             4                    0.774  -14.750
## 398 AR65K7A1187FB4DAA4             7                    0.810   -6.730
## 400 AR03BDP1187FB5B324             4                    0.978   -6.352
## 401 ARLE2071187FB3A270             4                    0.993   -4.691
## 412 ARS2P621187FB37B82             4                    1.000   -6.361
##       tempo tempo_confidence key key_confidence    energy pitch
## 395 125.718            0.900  10          0.180 0.4819015 0.007
## 397  90.068            0.823   6          0.511 0.3968982 0.003
## 398 138.835            0.359   8          0.283 0.9039595 0.016
## 400 127.813            1.000   1          0.258 0.8342521 0.003
## 401 147.973            0.062   8          0.762 0.5472997 0.004
## 412 106.966            0.857   8          0.751 0.6556737 0.008
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 395        6.226       47.207     -189.157      190.001     -138.238
## 397       10.492       48.584     -154.920      297.966     -161.924
## 398       16.216       58.017     -100.397      193.943      -85.133
## 400       33.317       55.484      -55.489      317.298     -131.288
## 401        8.526       59.523     -156.089      162.116     -113.271
## 412       19.419       56.339     -169.339      415.952     -191.764
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 395      143.856     -159.224      176.223      -26.293      122.506
## 397      121.138     -191.916      123.218      -69.500      114.867
## 398      209.996     -306.703      107.065      -64.077      107.915
## 400      166.195     -255.655       80.750      -66.332       87.790
## 401      144.326     -246.601      103.428      -62.105      135.361
## 412      168.583     -331.558      256.603      -73.666      164.419
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 395     -122.211      157.066      -86.574       88.179      -65.950
## 397      -77.121      115.117     -115.210       81.328     -143.760
## 398      -86.929       67.942     -104.582       87.691      -88.389
## 400     -131.519      223.668      -60.497       94.962      -62.392
## 401     -101.105      109.332     -100.504       72.557     -138.612
## 412     -114.593      254.808     -100.274       69.164     -154.189
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 395       89.287      -55.501       60.993      -51.407      111.417
## 397      140.287      -66.532       55.765      -80.902       75.827
## 398       68.173      -46.579       71.146      -63.987       60.030
## 400       69.739      -63.822       49.848      -42.340       61.869
## 401      110.278      -78.404       48.406      -54.959       68.578
## 412      162.825      -89.878       49.449      -95.860       99.688
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 395       -81.333        43.168       -46.490        48.067     1
## 397      -110.037        38.132       -55.501        55.654     1
## 398       -62.856        45.070       -49.433        58.311     1
## 400       -73.003        69.304       -42.080        37.477     1
## 401       -62.783       132.723       -41.585        60.525     1
## 412      -156.386        74.464       -50.067        46.874     1
##        .rnorm Top10.predict.proba Top10.predict Top10.fctr
## 395 0.6300986           0.2091810             0          1
## 397 0.6300986           0.3911361             0          1
## 398 0.6300986           0.1600492             0          1
## 400 0.6300986           0.2889788             0          1
## 401 0.6300986           0.2368477             0          1
## 412 0.6300986           0.1979844             0          1
##     Top10.predict.accurate             .label
## 395                  FALSE SOXFARA13B1C88A5B3
## 397                  FALSE SONEARZ13A99C14D6C
## 398                  FALSE SOFSRHH13775668108
## 400                  FALSE SOWDJOJ1373114BD21
## 401                  FALSE SOELKUA135BB837641
## 412                  FALSE SOMHVPR135C33B4BCD
##      year             songtitle      artistname             songID
## 2002 2006            Come To Me           Diddy SOOUWFZ12A58A7D940
## 3360 2003           Aerodynamic       Daft Punk SOHSQAW135C348F97C
## 3893 2002  I'm Gonna Be Alright  Jennifer Lopez SOKUEKL13762DFF90A
## 3973 2002        All You Wanted Michelle Branch SOFOCMF12B0B807DB7
## 4313 2001                 Again   Lenny Kravitz SOYFLDG12A6D4F8C2A
## 7347 1990 Rub You The Right Way     Johnny Gill SODTGKD12AB0185967
##                artistID timesignature timesignature_confidence loudness
## 2002 ARQ23MU1187FB3895B             4                    1.000   -7.113
## 3360 ARF8HTQ1187B9AE693             4                    1.000   -9.515
## 3893 AR7C6G11187B9B4C1E             4                    1.000   -6.257
## 3973 ART2QFA1187B9AC556             4                    1.000   -9.406
## 4313 ARFA4301187B9AD04A             4                    0.986   -6.410
## 7347 ARA6FM21187FB5A1EC             4                    1.000   -7.301
##        tempo tempo_confidence key key_confidence    energy pitch
## 2002  97.013            0.982   4          0.483 0.4610092 0.000
## 3360 122.858            0.827   7          0.483 0.8681445 0.003
## 3893  91.818            1.000   1          0.506 0.8248347 0.002
## 3973  96.115            0.976   8          0.369 0.7602917 0.002
## 4313  79.171            0.822   9          0.532 0.8067035 0.032
## 7347 112.228            1.000  10          0.702 0.8994925 0.007
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 2002       28.170       57.001     -130.808      208.256     -166.965
## 3360       29.387       53.327     -194.772      239.054      -86.103
## 3893        0.000       58.156        8.008      329.199     -207.981
## 3973        0.000       53.307      -67.565      171.130     -124.107
## 4313        0.045       56.851      -43.876      200.060     -154.120
## 7347       31.894       54.762      -28.900      257.134     -132.015
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 2002      118.162     -199.276      171.457     -123.897      127.520
## 3360      154.039     -101.172      247.645      -53.971      146.548
## 3893      102.534      -98.360      413.221      -54.120       83.395
## 3973      125.558     -106.372      154.827      -34.253      113.742
## 4313       99.553     -139.212      102.229      -89.530       71.542
## 7347      164.971     -291.022      255.750      -45.815      129.050
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 2002     -129.843      259.687     -103.947       84.287     -108.505
## 3360     -142.207      200.122     -105.266       74.768      -85.147
## 3893     -108.754       83.314      -84.462       97.982      -65.297
## 3973      -77.892       71.932      -83.294       49.729      -91.114
## 4313     -101.649       96.021      -63.890       56.727      -48.183
## 7347     -123.888      134.454      -66.643       74.146      -81.061
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 2002      136.401      -78.859       53.470      -55.100       89.498
## 3360       63.506      -50.344       82.090      -62.355       94.561
## 3893       84.563      -63.095       37.379      -50.288       94.975
## 3973       81.673      -52.876       52.835      -47.597       45.650
## 4313       83.825      -64.122       44.274      -39.910       44.762
## 7347       91.851      -62.983       70.162      -38.788       78.772
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 2002      -133.507        74.501       -76.573        51.817     1
## 3360       -83.596        36.813       -52.452        52.898     0
## 3893       -95.107        59.498       -49.669        48.162     1
## 3973      -104.165        72.624       -58.442        42.441     1
## 4313       -58.444        60.595       -48.750        50.906     1
## 7347       -97.458        79.368       -49.460        47.009     1
##         .rnorm Top10.predict.proba Top10.predict Top10.fctr
## 2002 0.6300986          0.36882102             0          1
## 3360 0.6300986          0.46008768             1          0
## 3893 0.6300986          0.10803419             0          1
## 3973 0.6300986          0.35207766             0          1
## 4313 0.6300986          0.02540754             0          1
## 7347 0.6300986          0.44542978             0          1
##      Top10.predict.accurate             .label
## 2002                  FALSE SOOUWFZ12A58A7D940
## 3360                  FALSE SOHSQAW135C348F97C
## 3893                  FALSE SOKUEKL13762DFF90A
## 3973                  FALSE SOFOCMF12B0B807DB7
## 4313                  FALSE SOYFLDG12A6D4F8C2A
## 7347                  FALSE SODTGKD12AB0185967
##      year                        songtitle      artistname
## 7536 1990 The Way You Do The Things You Do            UB40
## 7537 1990                     Ice Ice Baby     Vanilla Ice
## 7544 1990                       Cherry Pie         Warrant
## 7545 1990                       Release Me Wilson Phillips
## 7546 1990                          Hold On Wilson Phillips
## 7548 1990            Cockpit Dance Mixture             XTC
##                  songID           artistID timesignature
## 7536 SOBYHAX1311AFDCA86 AR1WWVL1187B9B0306             4
## 7537 SOJFSMW13741A466BA ARQUN9K1187B9B5BD7             4
## 7544 SOCHOMY13743D265F0 AR261YW1187FB374B3             4
## 7545 SOQNRIS13167715B05 ARY2W5X1187FB3CB7A             4
## 7546 SOLLKZM13775574E9D ARY2W5X1187FB3CB7A             4
## 7548 SOQUBXU136F0FFF403 ARNSYEO1187B9B8945             1
##      timesignature_confidence loudness   tempo tempo_confidence key
## 7536                    1.000   -9.914  80.865            0.941   8
## 7537                    0.326   -9.894 115.478            0.571   7
## 7544                    0.712   -4.324  88.066            0.288   1
## 7545                    1.000  -11.634  97.217            0.914   0
## 7546                    0.973   -7.034  97.808            0.976   5
## 7548                    0.419  -16.307 158.137            0.786   9
##      key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 7536          0.016 0.7241745 0.006       12.957       53.118      -27.565
## 7537          0.702 0.6837569 0.001        0.000       51.963     -173.377
## 7544          0.516 0.8985458 0.023        0.000       58.088      -16.488
## 7545          0.950 0.4776942 0.016        0.000       53.652     -150.628
## 7546          0.693 0.7144194 0.006        0.000       58.163     -111.940
## 7548          0.522 0.6787506 0.003        0.001       46.783      -64.676
##      timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 7536      206.216     -160.510      219.216     -244.082      118.664
## 7537      276.484     -241.572      123.640     -222.758      207.571
## 7544      231.833     -116.554      179.146     -338.931      279.202
## 7545      302.599     -112.474      169.783     -186.696      120.577
## 7546      262.005     -163.814      147.157     -105.837      184.883
## 7548      298.486     -202.746      197.763     -173.900      238.523
##      timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 7536      -71.090      125.652      -87.490       93.874      -95.402
## 7537      -68.497      113.034     -121.114      147.981      -97.509
## 7544      -62.531      115.599     -103.238      214.678      -75.943
## 7545      -31.180      152.829      -98.220      160.532      -97.204
## 7546      -32.457      117.092      -90.979      118.263     -106.717
## 7548     -113.570      128.029     -124.088      185.594     -107.018
##      timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 7536      103.980      -71.178       59.647      -45.392       78.779
## 7537       60.422     -154.418      149.028      -64.857       71.936
## 7544       72.145      -68.335      134.903      -82.790       53.873
## 7545       79.393      -99.681      116.012      -64.832       53.894
## 7546       86.727      -98.841       51.859      -71.843       51.896
## 7548       99.624     -100.962      104.866     -106.637       52.316
##      timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 7536      -48.077       60.752       -71.797        38.005       -43.463
## 7537      -96.373       87.181       -90.330        45.489       -71.101
## 7544      -49.426       97.366      -194.978       136.589       -49.064
## 7545      -55.841       69.711       -97.623        58.941       -52.319
## 7546      -52.159       57.414       -65.007        57.240       -55.734
## 7548      -53.908      112.356      -108.179        79.391       -73.005
##      timbre_11_max Top10    .rnorm Top10.predict.proba Top10.predict
## 7536        38.201     1 0.6300986          0.35093248             0
## 7537        42.939     1 0.6300986          0.31755307             0
## 7544        42.615     1 0.6300986          0.02874956             0
## 7545        65.427     1 0.6300986          0.18936059             0
## 7546        54.569     1 0.6300986          0.26275031             0
## 7548        72.490     0 0.6300986          0.45052829             1
##      Top10.fctr Top10.predict.accurate             .label
## 7536          1                  FALSE SOBYHAX1311AFDCA86
## 7537          1                  FALSE SOJFSMW13741A466BA
## 7544          1                  FALSE SOCHOMY13743D265F0
## 7545          1                  FALSE SOQNRIS13167715B05
## 7546          1                  FALSE SOLLKZM13775574E9D
## 7548          0                  FALSE SOQUBXU136F0FFF403
```

![](songs_files/figure-html/fit_training.all-23.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##               songID Top10 Top10.predict
## 1 SOBGGAB12C5664F054     0             0
## 2 SOPAQHU1315CD47F31     0             0
## 3 SOOIZOU1376E7C6386     0             0
## 4 SODRYWD1315CD49DBE     0             0
## 5 SOICMQB1315CD46EE3     0             0
## 6 SOCEYON1315CD4A23E     0             0
##                 songID Top10 Top10.predict
## 55  SOJSITU13A9DF07483     1             1
## 85  SOJMDHJ12B55270084     0             0
## 92  SOBRWQV12B3A138CDE     0             0
## 168 SOPEQSZ13740FDBA1E     1             0
## 225 SOWYCMV1315CD43489     0             0
## 251 SOCTMHJ13776551A5F     0             0
##                 songID Top10 Top10.predict
## 368 SOZHGUW137375147FE     0             0
## 369 SOQNGML13DC8B9A8A5     0             0
## 370 SOXGDEA1375A1AD2F8     0             0
## 371 SOEHNSK13DC4A18344     0             0
## 372 SOQOCRO13DC469AD34     0             0
## 373 SOPQHEH1374011CFB8     1             1
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.8490"
## [1] "probability threshold=0.4500"
##   Top10 Top10.predict.0 Top10.predict.1
## 1     0             309               5
## 2     1              40              19
## [1] "f.score.sel=0.4578"
##   Top10 Top10.preddmy.0
## 1     0             314
## 2     1              59
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "f.score.dmy=0.0000"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](songs_files/figure-html/predict_newdata-1.png) ![](songs_files/figure-html/predict_newdata-2.png) ![](songs_files/figure-html/predict_newdata-3.png) ![](songs_files/figure-html/predict_newdata-4.png) ![](songs_files/figure-html/predict_newdata-5.png) ![](songs_files/figure-html/predict_newdata-6.png) ![](songs_files/figure-html/predict_newdata-7.png) ![](songs_files/figure-html/predict_newdata-8.png) ![](songs_files/figure-html/predict_newdata-9.png) ![](songs_files/figure-html/predict_newdata-10.png) ![](songs_files/figure-html/predict_newdata-11.png) ![](songs_files/figure-html/predict_newdata-12.png) ![](songs_files/figure-html/predict_newdata-13.png) ![](songs_files/figure-html/predict_newdata-14.png) ![](songs_files/figure-html/predict_newdata-15.png) ![](songs_files/figure-html/predict_newdata-16.png) ![](songs_files/figure-html/predict_newdata-17.png) ![](songs_files/figure-html/predict_newdata-18.png) ![](songs_files/figure-html/predict_newdata-19.png) ![](songs_files/figure-html/predict_newdata-20.png) 

```
##    year                      songtitle        artistname
## 10 2010           Whataya Want From Me      Adam Lambert
## 24 2010 We Are The World 25: For Haiti Artists For Haiti
## 25 2010                          Magic             B.o.B
## 26 2010                      Airplanes             B.o.B
## 27 2010                 Nothin' On You             B.o.B
## 28 2010                              3    Britney Spears
##                songID           artistID timesignature
## 10 SOFOBLL13A9E22142F ARIGTAO11FED0C4411             4
## 24 SOTEGEI13134389BC0 ARZAYIQ1275322AEE7             4
## 25 SOCRJPX135FFE4F1AC ARKQQZA12086C116FC             4
## 26 SOVWJTF13B346C89A9 ARKQQZA12086C116FC             4
## 27 SOWHVEZ13672C4B6A4 ARKQQZA12086C116FC             4
## 28 SOIUHOE13730D0E5E8 AR03BDP1187FB5B324             4
##    timesignature_confidence loudness   tempo tempo_confidence key
## 10                    0.938  -15.557  95.904            0.929  11
## 24                    1.000   -6.218  75.018            0.232   4
## 25                    1.000   -4.281  82.543            0.948   6
## 26                    1.000   -4.632  93.041            0.943   9
## 27                    1.000   -4.310 104.015            1.000  10
## 28                    1.000   -4.880 129.933            0.476   1
##    key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 10          0.602 0.4259095 0.004       15.599       47.851      -83.614
## 24          0.709 0.7118709 0.008        0.000       58.297     -175.010
## 25          0.018 0.9318929 0.008        0.000       57.471     -113.300
## 26          0.000 0.8759300 0.009        8.040       57.416      -68.910
## 27          0.458 0.8848198 0.007        0.000       57.936     -140.194
## 28          0.429 0.6585794 0.004       32.417       56.879     -268.067
##    timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 10      231.735     -125.176      192.713     -164.046      211.905
## 24      171.130      -93.289      169.863     -151.942      216.772
## 25      173.531     -115.426      193.923     -171.550      155.595
## 26      172.242      -85.425      111.132     -189.283      319.509
## 27      207.768      -74.690      141.442     -367.307      131.140
## 28      201.653     -111.426      171.515     -208.238      330.762
##    timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 10      -54.926      113.781     -117.653      277.953      -80.263
## 24      -81.595      108.282      -84.914      144.482      -86.550
## 25      -75.683      109.680     -104.722      154.292      -75.857
## 26      -57.920      106.097      -85.214      105.962      -58.391
## 27      -56.402      153.979      -99.252      199.285      -90.684
## 28      -45.841      116.226      -82.732      188.653     -113.553
##    timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 10       76.129      -90.897       99.150      -69.346       56.233
## 24       92.991      -88.239       73.640      -66.116       70.950
## 25       65.525     -116.783       86.738      -66.377       45.026
## 26       62.698     -101.503      109.932      -58.905       41.963
## 27       71.062      -86.826       80.755      -60.000       47.343
## 28       81.334     -134.020       77.367      -58.482       74.102
##    timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 10      -94.733       62.789      -102.870        65.328       -48.411
## 24      -50.593       55.819      -108.325        51.494       -62.202
## 25      -51.316       76.215       -74.426        64.825       -60.809
## 26      -49.356       71.626      -110.716        50.365       -44.384
## 27      -68.064       55.142       -97.047        50.844       -51.398
## 28      -64.293       83.458      -198.739        91.060       -43.253
##    timbre_11_max Top10     .rnorm Top10.predict.proba Top10.predict
## 10        47.376     1 -0.2761841          0.37447749             0
## 24        55.321     1 -0.2761841          0.05658360             0
## 25        45.137     1 -0.2761841          0.16835317             0
## 26        29.936     1 -0.2761841          0.03793471             0
## 27        43.509     1 -0.2761841          0.17501595             0
## 28        45.054     1 -0.2761841          0.15388087             0
##    Top10.preddmy.proba Top10.preddmy Top10.fctr Top10.predict.accurate
## 10           0.1472018             0          1                  FALSE
## 24           0.1472018             0          1                  FALSE
## 25           0.1472018             0          1                  FALSE
## 26           0.1472018             0          1                  FALSE
## 27           0.1472018             0          1                  FALSE
## 28           0.1472018             0          1                  FALSE
##                .label
## 10 SOFOBLL13A9E22142F
## 24 SOTEGEI13134389BC0
## 25 SOCRJPX135FFE4F1AC
## 26 SOVWJTF13B346C89A9
## 27 SOWHVEZ13672C4B6A4
## 28 SOIUHOE13730D0E5E8
##     year            songtitle          artistname             songID
## 10  2010 Whataya Want From Me        Adam Lambert SOFOBLL13A9E22142F
## 129 2010        Young Forever  Jay-Z + Mr. Hudson SOWIARE13A94BE32A5
## 198 2010       Right Above It           Lil Wayne SOGEXCP135C23B66BC
## 269 2010                 Hard             Rihanna SOLSQLC13731ABDE72
## 320 2010                 Mine        Taylor Swift SOEMRLF1326F64E0FF
## 336 2010      Meet Me Halfway The Black Eyed Peas SOMKUCE1373E0304C4
##               artistID timesignature timesignature_confidence loudness
## 10  ARIGTAO11FED0C4411             4                    0.938  -15.557
## 129 ARJPIFW13B11B835B3             4                    0.841  -13.860
## 198 ARUDYKB11F4C83C269             4                    0.962   -4.875
## 269 ARKU3Z61187FB51DCA             4                    1.000   -5.973
## 320 ARS54I31187FB46721             4                    0.933   -6.212
## 336 ARTDQRC1187FB4EFD4             4                    1.000   -6.687
##       tempo tempo_confidence key key_confidence    energy pitch
## 10   95.904            0.929  11          0.602 0.4259095 0.004
## 129  72.348            0.292   9          0.698 0.4563160 0.005
## 198  75.988            0.433  11          0.018 0.8337179 0.004
## 269 130.054            0.894  11          0.684 0.8815718 0.006
## 320 121.029            0.820   7          0.923 0.6931062 0.001
## 336 130.026            0.800  11          0.637 0.5551177 0.004
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 10        15.599       47.851      -83.614      231.735     -125.176
## 129        8.099       48.620     -137.710      200.204     -101.514
## 198        6.468       57.182      -36.468      386.013     -111.036
## 269        0.349       56.536     -106.587      281.821     -187.247
## 320        0.010       56.939     -110.790      180.070     -138.626
## 336        0.000       57.816     -177.769      211.324     -139.685
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 10       192.713     -164.046      211.905      -54.926      113.781
## 129      189.216     -142.995      125.440      -59.853      101.023
## 198      139.824     -210.818      156.905      -43.392      141.387
## 269      226.528     -250.505      273.720      -74.642      127.575
## 320       81.994     -157.609      140.757      -56.590       88.454
## 336      115.174     -218.492      139.666      -66.526       97.211
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 10      -117.653      277.953      -80.263       76.129      -90.897
## 129      -84.743       99.109      -66.663       76.506      -92.493
## 198      -79.196       83.008     -112.244       69.845      -91.446
## 269     -154.622      141.457      -74.038       95.873     -109.799
## 320     -112.568      160.010      -55.498       80.704      -97.462
## 336     -108.700      133.487     -108.627       86.757      -96.976
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 10        99.150      -69.346       56.233      -94.733       62.789
## 129      107.557      -62.809       54.336      -65.655      102.730
## 198      100.131      -61.257       41.830      -51.863       63.472
## 269      100.552      -62.639       54.530      -85.890       82.944
## 320      103.517      -54.245       38.323      -55.244       73.445
## 336      136.198      -79.597       52.320      -63.056       78.526
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 10       -102.870        65.328       -48.411        47.376     1
## 129       -73.457        48.407       -62.820        49.839     1
## 198       -84.703        40.672       -70.747        43.031     1
## 269      -160.967        50.229       -66.591        66.126     1
## 320       -73.344        49.825       -45.588        35.956     1
## 336      -103.140        51.029       -45.295        69.895     1
##         .rnorm Top10.predict.proba Top10.predict Top10.preddmy.proba
## 10  -0.2761841          0.37447749             0           0.1472018
## 129 -0.2761841          0.25888600             0           0.1472018
## 198 -0.2761841          0.43926276             0           0.1472018
## 269 -0.2761841          0.27458093             0           0.1472018
## 320 -0.2761841          0.09579406             0           0.1472018
## 336 -0.2761841          0.13510643             0           0.1472018
##     Top10.preddmy Top10.fctr Top10.predict.accurate             .label
## 10              0          1                  FALSE SOFOBLL13A9E22142F
## 129             0          1                  FALSE SOWIARE13A94BE32A5
## 198             0          1                  FALSE SOGEXCP135C23B66BC
## 269             0          1                  FALSE SOLSQLC13731ABDE72
## 320             0          1                  FALSE SOEMRLF1326F64E0FF
## 336             0          1                  FALSE SOMKUCE1373E0304C4
##     year                 songtitle          artistname             songID
## 332 2010                   Imma Be The Black Eyed Peas SOINQJJ137400E5B55
## 333 2010      The Time (Dirty Bit) The Black Eyed Peas SOBYLLZ137539A7718
## 335 2010           I Gotta Feeling The Black Eyed Peas SOAHVAI13730FE870F
## 336 2010           Meet Me Halfway The Black Eyed Peas SOMKUCE1373E0304C4
## 361 2010                   Say Aah          Trey Songz SOMWCCA130516DF0FC
## 363 2010 DJ Got Us Fallin' In Love               Usher SORZYEE1373FE4DA11
##               artistID timesignature timesignature_confidence loudness
## 332 ARTDQRC1187FB4EFD4             3                    1.000  -12.114
## 333 ARTDQRC1187FB4EFD4             3                    0.967   -5.226
## 335 ARTDQRC1187FB4EFD4             4                    0.939   -4.735
## 336 ARTDQRC1187FB4EFD4             4                    1.000   -6.687
## 361 ARL60FF1187B9AE52D             4                    0.845   -3.820
## 363 ARPDVPJ1187B9ADBE9             4                    1.000   -6.084
##       tempo tempo_confidence key key_confidence    energy pitch
## 332  82.497            0.332   6          0.035 0.4514489 0.001
## 333  86.705            0.250  11          0.295 0.9493676 0.001
## 335 127.982            1.000   7          0.221 0.8223174 0.006
## 336 130.026            0.800  11          0.637 0.5551177 0.004
## 361  93.176            0.860   1          0.923 0.9085875 0.005
## 363 120.978            0.820  10          0.374 0.8811846 0.007
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 332       19.407       51.626     -130.310      231.110     -190.719
## 333       26.168       56.195     -212.989      255.041     -113.090
## 335        0.000       58.972      -66.968      235.864     -118.711
## 336        0.000       57.816     -177.769      211.324     -139.685
## 361        0.000       59.127     -173.464      305.440     -130.982
## 363       26.915       56.863     -124.451      191.281      -95.545
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 332      105.888     -170.822      301.544      -62.865      125.001
## 333      165.360     -233.532      425.867     -103.232      100.335
## 335      161.018     -132.146      275.025      -58.483      122.456
## 336      115.174     -218.492      139.666      -66.526       97.211
## 361      188.841     -237.298      153.018      -86.753      151.261
## 363      154.013     -247.008      127.992      -60.448       93.126
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 332     -208.399      137.571      -93.516       76.434     -110.735
## 333     -246.401      297.424     -102.307      114.413     -118.030
## 335     -107.801      183.373     -111.455       93.630     -103.996
## 336     -108.700      133.487     -108.627       86.757      -96.976
## 361     -111.019      143.723      -72.207       89.148      -87.810
## 363     -112.956      260.388     -107.440       75.987      -94.903
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 332      116.489      -64.822       63.320      -81.776       82.988
## 333       98.511      -85.597       56.245      -60.735       64.614
## 335      115.687      -60.497       40.170      -48.145      110.292
## 336      136.198      -79.597       52.320      -63.056       78.526
## 361       95.220      -86.576       45.987      -58.968       89.324
## 363      119.859      -69.901       40.773      -82.396       74.194
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 332      -130.265        49.348       -60.305        37.796     1
## 333      -115.136        85.240       -66.088        58.374     1
## 335      -140.355        47.433       -49.262        32.136     1
## 336      -103.140        51.029       -45.295        69.895     1
## 361      -109.011        60.731       -46.085        75.635     1
## 363      -120.622        53.183       -55.887        42.752     1
##         .rnorm Top10.predict.proba Top10.predict Top10.preddmy.proba
## 332 -0.2761841           0.2814858             0           0.1472018
## 333 -0.2761841           0.3127534             0           0.1472018
## 335 -0.2761841           0.1017517             0           0.1472018
## 336 -0.2761841           0.1351064             0           0.1472018
## 361 -0.2761841           0.1062336             0           0.1472018
## 363 -0.2761841           0.2512722             0           0.1472018
##     Top10.preddmy Top10.fctr Top10.predict.accurate             .label
## 332             0          1                  FALSE SOINQJJ137400E5B55
## 333             0          1                  FALSE SOBYLLZ137539A7718
## 335             0          1                  FALSE SOAHVAI13730FE870F
## 336             0          1                  FALSE SOMKUCE1373E0304C4
## 361             0          1                  FALSE SOMWCCA130516DF0FC
## 363             0          1                  FALSE SORZYEE1373FE4DA11
```

![](songs_files/figure-html/predict_newdata-21.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ROCR_1.0-6      gplots_2.16.0   mgcv_1.8-4      nlme_3.1-120   
## [5] reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13     survival_2.38-1
## [9] ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       caTools_1.17.1     colorspace_1.2-6  
##  [4] digest_0.6.8       evaluate_0.5.5     formatR_1.0       
##  [7] gdata_2.13.3       grid_3.1.3         gtable_0.1.2      
## [10] gtools_3.4.1       htmltools_0.2.6    KernSmooth_2.23-14
## [13] knitr_1.9          labeling_0.3       lattice_0.20-30   
## [16] MASS_7.3-39        Matrix_1.1-5       munsell_0.4.2     
## [19] proto_0.3-10       Rcpp_0.11.5        rmarkdown_0.5.1   
## [22] scales_0.2.4       splines_3.1.3      stringr_0.6.2     
## [25] tools_3.1.3        yaml_2.1.13
```
