
TVM <- read.csv(file="D:/R Input files/TVM Working File.csv",header = T)

library("tm")
library("tmap")
library("stringr")
TVM$Description_Fin   <- toupper(TVM$Description)
TVM$Description_Fin   <- removePunctuation(TVM$Description_Fin) # remove %^&*()~!@#${}_+:"<>?,./;'[]-= #
TVM$Description_Fin   <- stripWhitespace(TVM$Description_Fin)

#TVM$Description_Fin   <- removeWords(TVM$Description_Fin,c("Â","???"))   #  ???"~ #
#TVM$gokul <- str_replace_all(TVM$gokul, "[[:punct:]]","")       #(The base R equivalent is gsub("[[:punct:]]", " ", x).)
#TVM$gokul <- str_replace_all(TVM$gokul, "[^[:alnum:]]", " ")
#TVM$gokul <- str_replace_all(TVM$gokul, ["Â","???"],"")

# FLAG Fields #
TVM$ABLEFLAG	                  <-   	ifelse(grepl("ABLE",TVM$Description_Fin),1,0)
TVM$ACCESSFLAG	                <-   	ifelse(grepl("ACCESS",TVM$Description_Fin),1,0)
TVM$ACCOUNTFLAG	                <-   	ifelse(grepl("ACCOUNT",TVM$Description_Fin),1,0)
TVM$ACROBATFLAG	                <-   	ifelse(grepl("ACROBAT",TVM$Description_Fin),1,0)
TVM$ACTIVATEFLAG	              <-   	ifelse(grepl("ACTIVATE",TVM$Description_Fin),1,0)
TVM$ACUTECAREFLAG	              <-   	ifelse(grepl("ACUTE CARE",TVM$Description_Fin),1,0)
TVM$ADAPTERSFLAG	              <-   	ifelse(grepl("ADAPTERS",TVM$Description_Fin),1,0)
TVM$ADDFLAG	                    <-   	ifelse(grepl("ADD",TVM$Description_Fin),1,0)
TVM$ADDRESSFLAG	                <-   	ifelse(grepl("ADDRESS",TVM$Description_Fin),1,0)
TVM$ADMINFLAG	                  <-   	ifelse(grepl("ADMIN",TVM$Description_Fin),1,0)
TVM$ADOBEFLAG	                  <-   	ifelse(grepl("ADOBE",TVM$Description_Fin),1,0)
TVM$ADPFLAG	                    <-   	ifelse(grepl("ADP",TVM$Description_Fin),1,0)
TVM$AEFLAG	                    <-   	ifelse(grepl("AE",TVM$Description_Fin),1,0)
TVM$AGENTIDFLAG	                <-   	ifelse(grepl("AGENT ID",TVM$Description_Fin),1,0)
TVM$AIRSFLAG	                  <-   	ifelse(grepl("AIRS",TVM$Description_Fin),1,0)
TVM$AIRWATCHFLAG	              <-   	ifelse(grepl("AIRWATCH",TVM$Description_Fin),1,0)
TVM$ALERTFLAG	                  <-   	ifelse(grepl("ALERT",TVM$Description_Fin),1,0)
TVM$ALLFLAG	                    <-   	ifelse(grepl("ALL",TVM$Description_Fin),1,0)
TVM$ALLANFLAG	                  <-   	ifelse(grepl("ALLAN",TVM$Description_Fin),1,0)
TVM$ALLOWFLAG	                  <-   	ifelse(grepl("ALLOW",TVM$Description_Fin),1,0)
TVM$ALLSCRIPTSFLAG	                <-   	ifelse(grepl("ALLSCRIPTS",TVM$Description_Fin),1,0)
TVM$ALLUSERFLAG	                <-   	ifelse(grepl("ALL USER",TVM$Description_Fin),1,0)
TVM$ALTERFLAG	                <-   	ifelse(grepl("ALTER",TVM$Description_Fin),1,0)
TVM$ALTIUMFLAG	                <-   	ifelse(grepl("ALTIUM",TVM$Description_Fin),1,0)
TVM$ANONYMOUSFLAG	                <-   	ifelse(grepl("ANONYMOUS",TVM$Description_Fin),1,0)
TVM$APACHEFLAG	                <-   	ifelse(grepl("APACHE",TVM$Description_Fin),1,0)
TVM$APPFLAG	                <-   	ifelse(grepl("APP",TVM$Description_Fin),1,0)
TVM$APPLICATIONFLAG	                <-   	ifelse(grepl("APPLICATION",TVM$Description_Fin),1,0)
TVM$APPOINTMENTFLAG	                <-   	ifelse(grepl("APPOINTMENT",TVM$Description_Fin),1,0)
TVM$APPROVFLAG	                <-   	ifelse(grepl("APPROV",TVM$Description_Fin),1,0)
TVM$APPROVALFLAG	                <-   	ifelse(grepl("APPROVAL",TVM$Description_Fin),1,0)
TVM$AREFLAG	                <-   	ifelse(grepl("ARE",TVM$Description_Fin),1,0)
TVM$AREAFLAG	                <-   	ifelse(grepl("AREA",TVM$Description_Fin),1,0)
TVM$ARENTFLAG	                <-   	ifelse(grepl("ARENT",TVM$Description_Fin),1,0)
TVM$AS400FLAG	                <-   	ifelse(grepl("AS400",TVM$Description_Fin),1,0)
TVM$ASAPFLAG	                <-   	ifelse(grepl("ASAP",TVM$Description_Fin),1,0)
TVM$ASSEMBLYFLAG	                <-   	ifelse(grepl("ASSEMBLY",TVM$Description_Fin),1,0)
TVM$ASSETFLAG	                <-   	ifelse(grepl("ASSET",TVM$Description_Fin),1,0)
TVM$ASTEAFLAG	                <-   	ifelse(grepl("ASTEA",TVM$Description_Fin),1,0)
TVM$ATSFLAG	                <-   	ifelse(grepl("ATS",TVM$Description_Fin),1,0)
TVM$ATTACHMENTFLAG	                <-   	ifelse(grepl("ATTACHMENT",TVM$Description_Fin),1,0)
TVM$ATTACKFLAG	                <-   	ifelse(grepl("ATTACK",TVM$Description_Fin),1,0)
TVM$ATTEMPFLAG	                <-   	ifelse(grepl("ATTEMP",TVM$Description_Fin),1,0)
TVM$ATTEMPTSFLAG	                <-   	ifelse(grepl("ATTEMPTS",TVM$Description_Fin),1,0)
TVM$AUTHFLAG	                <-   	ifelse(grepl("AUTH",TVM$Description_Fin),1,0)
TVM$AUTHENTICATEFLAG	                <-   	ifelse(grepl("AUTHENTICATE",TVM$Description_Fin),1,0)
TVM$AUTHORIZATIONFLAG	                <-   	ifelse(grepl("AUTHORIZATION",TVM$Description_Fin),1,0)
TVM$AUTHORIZEFLAG	                <-   	ifelse(grepl("AUTHORIZE",TVM$Description_Fin),1,0)
TVM$AUTHORFLAG	                <-   	ifelse(grepl("AUTHOR",TVM$Description_Fin),1,0)
TVM$AUTOFLAG	                <-   	ifelse(grepl("AUTO",TVM$Description_Fin),1,0)
TVM$AVAILABLEFLAG	                <-   	ifelse(grepl("AVAILABLE",TVM$Description_Fin),1,0)
TVM$AVAYAFLAG	                <-   	ifelse(grepl("AVAYA",TVM$Description_Fin),1,0)
TVM$AVOIDFLAG	                <-   	ifelse(grepl("AVOID",TVM$Description_Fin),1,0)
TVM$BACKINFLAG	                <-   	ifelse(grepl("BACKIN",TVM$Description_Fin),1,0)
TVM$BACKINGFLAG	                <-   	ifelse(grepl("BACKING",TVM$Description_Fin),1,0)
TVM$BACKINGUPFLAG	                <-   	ifelse(grepl("BACKINGUP",TVM$Description_Fin),1,0)
TVM$BACKUPFLAG	                <-   	ifelse(grepl("BACKUP",TVM$Description_Fin),1,0)
TVM$BADFLAG	                <-   	ifelse(grepl("BAD",TVM$Description_Fin),1,0)
TVM$BARCODEFLAG	                <-   	ifelse(grepl("BAR CODE",TVM$Description_Fin),1,0)
TVM$BARCODEFLAG2	                <-   	ifelse(grepl("BARCODE",TVM$Description_Fin),1,0)
TVM$BASEFLAG	                <-   	ifelse(grepl("BASE",TVM$Description_Fin),1,0)
TVM$BATTERYFLAG	                <-   	ifelse(grepl("BATTERY",TVM$Description_Fin),1,0)
TVM$BEEPFLAG	                <-   	ifelse(grepl("BEEP",TVM$Description_Fin),1,0)
TVM$BILLFLAG	                <-   	ifelse(grepl("BILL",TVM$Description_Fin),1,0)
TVM$BINSFLAG	                <-   	ifelse(grepl("BINS",TVM$Description_Fin),1,0)
TVM$BLANKFLAG	                <-   	ifelse(grepl("BLANK",TVM$Description_Fin),1,0)
TVM$BLOCKFLAG	                <-   	ifelse(grepl("BLOCK",TVM$Description_Fin),1,0)
TVM$BLURFLAG	                <-   	ifelse(grepl("BLUR",TVM$Description_Fin),1,0)
TVM$BOMFLAG	                <-   	ifelse(grepl("BOM",TVM$Description_Fin),1,0)
TVM$BOOKFLAG	                <-   	ifelse(grepl("BOOK",TVM$Description_Fin),1,0)
TVM$BOOTINGFLAG	                <-   	ifelse(grepl("BOOTING",TVM$Description_Fin),1,0)
TVM$BOXFLAG	                <-   	ifelse(grepl("BOX",TVM$Description_Fin),1,0)
TVM$BOXINGFLAG	                <-   	ifelse(grepl("BOXING",TVM$Description_Fin),1,0)
TVM$BROKEFLAG	                <-   	ifelse(grepl("BROKE",TVM$Description_Fin),1,0)
TVM$BROKENFLAG	                <-   	ifelse(grepl("BROKEN",TVM$Description_Fin),1,0)
TVM$BROOMFIELDFLAG	                <-   	ifelse(grepl("BROOMFIELD",TVM$Description_Fin),1,0)
TVM$BUGFLAG	                <-   	ifelse(grepl("BUG",TVM$Description_Fin),1,0)
TVM$BUILDINGFLAG	                <-   	ifelse(grepl("BUILDING",TVM$Description_Fin),1,0)
TVM$BULLETINFLAG	                <-   	ifelse(grepl("BULLETIN",TVM$Description_Fin),1,0)
TVM$BWFLAG	                <-   	ifelse(grepl("BW",TVM$Description_Fin),1,0)
TVM$CALENDARFLAG	                <-   	ifelse(grepl("CALENDAR",TVM$Description_Fin),1,0)
TVM$CABLEFLAG	                <-   	ifelse(grepl("CABLE",TVM$Description_Fin),1,0)
TVM$CADFLAG	                <-   	ifelse(grepl("CAD",TVM$Description_Fin),1,0)
TVM$CALIBRATFLAG	                <-   	ifelse(grepl("CALIBRAT",TVM$Description_Fin),1,0)
TVM$CALLCENTERFLAG	                <-   	ifelse(grepl("CALL CENTER",TVM$Description_Fin),1,0)
TVM$CALLFLAG	                <-   	ifelse(grepl("CALL",TVM$Description_Fin),1,0)
TVM$CANCELFLAG	                <-   	ifelse(grepl("CANCEL",TVM$Description_Fin),1,0)
TVM$CANNOTFLAG	                <-   	ifelse(grepl("CANNOT",TVM$Description_Fin),1,0)
TVM$CANNOTFLAG2	                <-   	ifelse(grepl("CAN NOT",TVM$Description_Fin),1,0)
TVM$CANNOTGETFLAG	                <-   	ifelse(grepl("CAN NOT GET",TVM$Description_Fin),1,0)
TVM$CANTFLAG	                <-   	ifelse(grepl("CANT",TVM$Description_Fin),1,0)
TVM$CANTGETFLAG	                <-   	ifelse(grepl("CANT GET",TVM$Description_Fin),1,0)
TVM$CARFLAG	                <-   	ifelse(grepl("CAR",TVM$Description_Fin),1,0)
TVM$CARTFLAG	                <-   	ifelse(grepl("CART",TVM$Description_Fin),1,0)
TVM$CARTRIDGEFLAG	                <-   	ifelse(grepl("CARTRIDGE",TVM$Description_Fin),1,0)
TVM$CASEFLAG	                <-   	ifelse(grepl("CASE",TVM$Description_Fin),1,0)
TVM$CENSUSFLAG	                <-   	ifelse(grepl("CENSUS",TVM$Description_Fin),1,0)
TVM$CERNERFLAG	                <-   	ifelse(grepl("CERNER",TVM$Description_Fin),1,0)
TVM$CHAINFLAG	                <-   	ifelse(grepl("CHAIN",TVM$Description_Fin),1,0)
TVM$CHANGEFLAG	                <-   	ifelse(grepl("CHANGE",TVM$Description_Fin),1,0)
TVM$CHARGFLAG	                <-   	ifelse(grepl("CHARG",TVM$Description_Fin),1,0)
TVM$CHARTFLAG	                <-   	ifelse(grepl("CHART",TVM$Description_Fin),1,0)
TVM$CHECKFLAG	                <-   	ifelse(grepl("CHECK",TVM$Description_Fin),1,0)
TVM$CHINESEFLAG	                <-   	ifelse(grepl("CHINESE",TVM$Description_Fin),1,0)
TVM$CISCOFLAG	                <-   	ifelse(grepl("CISCO",TVM$Description_Fin),1,0)
TVM$CITRIXFLAG	                <-   	ifelse(grepl("CITRIX",TVM$Description_Fin),1,0)
TVM$CLOCKFLAG	                <-   	ifelse(grepl("CLOCK",TVM$Description_Fin),1,0)
TVM$CMBFLAG	                <-   	ifelse(grepl("CMB",TVM$Description_Fin),1,0)
TVM$CODEFLAG	                <-   	ifelse(grepl("CODE",TVM$Description_Fin),1,0)
TVM$COMINGFLAG	                <-   	ifelse(grepl("COMING",TVM$Description_Fin),1,0)
TVM$COMMUNICATFLAG	                <-   	ifelse(grepl("COMMUNICAT",TVM$Description_Fin),1,0)
TVM$COMMUNICATIONFLAG	                <-   	ifelse(grepl("COMMUNICATION",TVM$Description_Fin),1,0)
TVM$COMMUNICATORFLAG	                <-   	ifelse(grepl("COMMUNICATOR",TVM$Description_Fin),1,0)
TVM$COMPANYFLAG	                <-   	ifelse(grepl("COMPANY",TVM$Description_Fin),1,0)
TVM$COMPUTERFLAG	                <-   	ifelse(grepl("COMPUTER",TVM$Description_Fin),1,0)
TVM$COMPUTERSFLAG	                <-   	ifelse(grepl("COMPUTERS",TVM$Description_Fin),1,0)
TVM$CONFERENCEFLAG	                <-   	ifelse(grepl("CONFERENCE",TVM$Description_Fin),1,0)
TVM$CONFIGFLAG	                <-   	ifelse(grepl("CONFIG",TVM$Description_Fin),1,0)
TVM$CONFIRMATIONFLAG	                <-   	ifelse(grepl("CONFIRMATION",TVM$Description_Fin),1,0)
TVM$CONNECTFLAG	                <-   	ifelse(grepl("CONNECT",TVM$Description_Fin),1,0)
TVM$CONNECTIONFLAG	                <-   	ifelse(grepl("CONNECTION",TVM$Description_Fin),1,0)
TVM$CONNECTIVITYFLAG	                <-   	ifelse(grepl("CONNECTIVITY",TVM$Description_Fin),1,0)
TVM$CONSULTANTSFLAG	                <-   	ifelse(grepl("CONSULTANTS",TVM$Description_Fin),1,0)
TVM$CONTACTFLAG	                <-   	ifelse(grepl("CONTACT",TVM$Description_Fin),1,0)
TVM$CONTRACTFLAG	                <-   	ifelse(grepl("CONTRACT",TVM$Description_Fin),1,0)
TVM$CONTROLLERFLAG	                <-   	ifelse(grepl("CONTROLLER",TVM$Description_Fin),1,0)
TVM$COPIERFLAG	                <-   	ifelse(grepl("COPIER",TVM$Description_Fin),1,0)
TVM$COPYFLAG	                <-   	ifelse(grepl("COPY",TVM$Description_Fin),1,0)
TVM$COPYMACHINEFLAG	                <-   	ifelse(grepl("COPY MACHINE",TVM$Description_Fin),1,0)
TVM$CORDINGMACHINEFLAG	                <-   	ifelse(grepl("CORDING MACHINE",TVM$Description_Fin),1,0)
TVM$CORRECTFLAG	                <-   	ifelse(grepl("CORRECT",TVM$Description_Fin),1,0)
TVM$CPUFLAG	                <-   	ifelse(grepl("CPU",TVM$Description_Fin),1,0)
TVM$CRASHFLAG	                <-   	ifelse(grepl("CRASH",TVM$Description_Fin),1,0)
TVM$CREATFLAG	                <-   	ifelse(grepl("CREAT",TVM$Description_Fin),1,0)
TVM$CREATEFLAG	                <-   	ifelse(grepl("CREATE",TVM$Description_Fin),1,0)
TVM$CREATIONFLAG	                <-   	ifelse(grepl("CREATION",TVM$Description_Fin),1,0)
TVM$CRITICALFLAG	                <-   	ifelse(grepl("CRITICAL",TVM$Description_Fin),1,0)
TVM$CROSSFLAG	                <-   	ifelse(grepl("CROSS",TVM$Description_Fin),1,0)
TVM$CSRSFLAG	                <-   	ifelse(grepl("CSRS",TVM$Description_Fin),1,0)
TVM$CUSTOMERFLAG	                <-   	ifelse(grepl("CUSTOMER",TVM$Description_Fin),1,0)
TVM$CUTFLAG	                <-   	ifelse(grepl("CUT",TVM$Description_Fin),1,0)
TVM$DAMAGEFLAG	                <-   	ifelse(grepl("DAMAGE",TVM$Description_Fin),1,0)
TVM$DAMAGFLAG	                <-   	ifelse(grepl("DAMAG",TVM$Description_Fin),1,0)
TVM$DASHBOARDFLAG	                <-   	ifelse(grepl("DASHBOARD",TVM$Description_Fin),1,0)
TVM$DATABASEFLAG	                <-   	ifelse(grepl("DATABASE",TVM$Description_Fin),1,0)
TVM$DATAFLAG	                <-   	ifelse(grepl("DATA",TVM$Description_Fin),1,0)
TVM$DATEFLAG	                <-   	ifelse(grepl("DATE",TVM$Description_Fin),1,0)
TVM$DAYSFLAG	                <-   	ifelse(grepl("DAYS",TVM$Description_Fin),1,0)
TVM$DEACTIVATIONFLAG	                <-   	ifelse(grepl("DEACTIVATION",TVM$Description_Fin),1,0)
TVM$DEADFLAG	                <-   	ifelse(grepl("DEAD",TVM$Description_Fin),1,0)
TVM$DEALERLOCATORFLAG	                <-   	ifelse(grepl("DEALER LOCATOR",TVM$Description_Fin),1,0)
TVM$DEBUGFLAG	                <-   	ifelse(grepl("DEBUG",TVM$Description_Fin),1,0)
TVM$DEFAULTFLAG	                <-   	ifelse(grepl("DEFAULT",TVM$Description_Fin),1,0)
TVM$DELAYFLAG	                <-   	ifelse(grepl("DELAY",TVM$Description_Fin),1,0)
TVM$DELETFLAG	                <-   	ifelse(grepl("DELET",TVM$Description_Fin),1,0)
TVM$DELIVERFLAG	                <-   	ifelse(grepl("DELIVER",TVM$Description_Fin),1,0)
TVM$DELLFLAG	                <-   	ifelse(grepl("DELL",TVM$Description_Fin),1,0)
TVM$DEMANDFLAG	                <-   	ifelse(grepl("DEMAND",TVM$Description_Fin),1,0)
TVM$DENIEDFLAG	                <-   	ifelse(grepl("DENIED",TVM$Description_Fin),1,0)
TVM$DENTIRXFLAG	                <-   	ifelse(grepl("DENTIRX",TVM$Description_Fin),1,0)
TVM$DENTRIXFLAG	                <-   	ifelse(grepl("DENTRIX",TVM$Description_Fin),1,0)
TVM$DENVERFLAG	                <-   	ifelse(grepl("DENVER",TVM$Description_Fin),1,0)
TVM$DESKFLAG	                <-   	ifelse(grepl("DESK",TVM$Description_Fin),1,0)
TVM$DESKTOPFLAG	                <-   	ifelse(grepl("DESKTOP",TVM$Description_Fin),1,0)
TVM$DETAILSFLAG	                <-   	ifelse(grepl("DETAILS",TVM$Description_Fin),1,0)
TVM$DETECTFLAG	                <-   	ifelse(grepl("DETECT",TVM$Description_Fin),1,0)
TVM$DEVFLAG	                <-   	ifelse(grepl("DEV",TVM$Description_Fin),1,0)
TVM$DEVICEFLAG	                <-   	ifelse(grepl("DEVICE",TVM$Description_Fin),1,0)
TVM$DHCPFLAG	                <-   	ifelse(grepl("DHCP",TVM$Description_Fin),1,0)
TVM$DIAGNOSFLAG	                <-   	ifelse(grepl("DIAGNOS",TVM$Description_Fin),1,0)
TVM$DIALFLAG	                <-   	ifelse(grepl("DIAL",TVM$Description_Fin),1,0)
TVM$DIDFLAG	                <-   	ifelse(grepl("DID",TVM$Description_Fin),1,0)
TVM$DIRECTFLAG	                <-   	ifelse(grepl("DIRECT",TVM$Description_Fin),1,0)
TVM$DIRECTCONNECTFLAG	                <-   	ifelse(grepl("DIRECT CONNECT",TVM$Description_Fin),1,0)
TVM$DIRECTWEBFLAG	                <-   	ifelse(grepl("DIRECT WEB",TVM$Description_Fin),1,0)
TVM$DIRECTWEBFLAG2	                <-   	ifelse(grepl("DIRECTWEB",TVM$Description_Fin),1,0)
TVM$DISABLEFLAG	                <-   	ifelse(grepl("DISABLE",TVM$Description_Fin),1,0)
TVM$DISCHARGEFLAG	                <-   	ifelse(grepl("DISCHARGE",TVM$Description_Fin),1,0)
TVM$DISKFLAG	                <-   	ifelse(grepl("DISK",TVM$Description_Fin),1,0)
TVM$DOCFLAG	                <-   	ifelse(grepl("DOC",TVM$Description_Fin),1,0)
TVM$DOCUMENTFLAG	                <-   	ifelse(grepl("DOCUMENT",TVM$Description_Fin),1,0)
TVM$DOESNOTFLAG	                <-   	ifelse(grepl("DOES NOT",TVM$Description_Fin),1,0)
TVM$DOESNOTWORKFLAG	                <-   	ifelse(grepl("DOES NOT WORK",TVM$Description_Fin),1,0)
TVM$DOESNTFEEDFLAG	                <-   	ifelse(grepl("DOESNT FEED",TVM$Description_Fin),1,0)
TVM$DOESNTFLAG	                <-   	ifelse(grepl("DOESNT",TVM$Description_Fin),1,0)
TVM$DOESNTWORKFLAG	                <-   	ifelse(grepl("DOESNT WORK",TVM$Description_Fin),1,0)
TVM$DOFLAG	                <-   	ifelse(grepl("DO",TVM$Description_Fin),1,0)
TVM$DOMAINFLAG	                <-   	ifelse(grepl("DOMAIN",TVM$Description_Fin),1,0)
TVM$DONGLEFLAG	                <-   	ifelse(grepl("DONGLE",TVM$Description_Fin),1,0)
TVM$DOSAGEFLAG	                <-   	ifelse(grepl("DOSAGE",TVM$Description_Fin),1,0)
TVM$DOWNFLAG	                <-   	ifelse(grepl("DOWN",TVM$Description_Fin),1,0)
TVM$DOWNLOADFLAG	                <-   	ifelse(grepl("DOWNLOAD",TVM$Description_Fin),1,0)
TVM$DOWNSTAIRFLAG	                <-   	ifelse(grepl("DOWNSTAIR",TVM$Description_Fin),1,0)
TVM$DRIVEFLAG	                <-   	ifelse(grepl("DRIVE",TVM$Description_Fin),1,0)
TVM$DRIVESFLAG	                <-   	ifelse(grepl("DRIVES",TVM$Description_Fin),1,0)
TVM$DROPBOXFLAG	                <-   	ifelse(grepl("DROPBOX",TVM$Description_Fin),1,0)
TVM$DROPFLAG	                <-   	ifelse(grepl("DROP",TVM$Description_Fin),1,0)
TVM$ECCFLAG	                <-   	ifelse(grepl("ECC",TVM$Description_Fin),1,0)
TVM$ECLIPSYSFLAG	                <-   	ifelse(grepl("ECLIPSYS",TVM$Description_Fin),1,0)
TVM$ECLIPSYSFLAG2	                <-   	ifelse(grepl("ECLIPS",TVM$Description_Fin),1,0)
TVM$ECLIPSYSFLAG3	                <-   	ifelse(grepl("ECLYPSIS",TVM$Description_Fin),1,0)
TVM$EDITFLAG	                <-   	ifelse(grepl("EDIT",TVM$Description_Fin),1,0)
TVM$EFTFLAG	                <-   	ifelse(grepl("EFT",TVM$Description_Fin),1,0)
TVM$EKGFLAG	                <-   	ifelse(grepl("EKG",TVM$Description_Fin),1,0)
TVM$ELINKFLAG	                <-   	ifelse(grepl("ELINK",TVM$Description_Fin),1,0)
TVM$ELINKFLAG2	                <-   	ifelse(grepl("E LINK",TVM$Description_Fin),1,0)
TVM$ELINKFLAG3	                <-   	ifelse(grepl("E-LINK",TVM$Description_Fin),1,0)
TVM$EMAILFLAG	                <-   	ifelse(grepl("EMAIL",TVM$Description_Fin),1,0)
TVM$EMAILFLAG2	                <-   	ifelse(grepl("E-MAIL",TVM$Description_Fin),1,0)
TVM$EMAILFLAG3	                <-   	ifelse(grepl("E MAIL",TVM$Description_Fin),1,0)
TVM$EMAILFLAG4	                <-   	ifelse(grepl("E -MAIL",TVM$Description_Fin),1,0)
TVM$EMPLOYEEFLAG	                <-   	ifelse(grepl("EMPLOYEE",TVM$Description_Fin),1,0)
TVM$ENERGYFLAG	                <-   	ifelse(grepl("ENERGY",TVM$Description_Fin),1,0)
TVM$ENGINEERFLAG	                <-   	ifelse(grepl("ENGINEER",TVM$Description_Fin),1,0)
TVM$ENTERFLAG	                <-   	ifelse(grepl("ENTER",TVM$Description_Fin),1,0)
TVM$ENTERPRISEFLAG	                <-   	ifelse(grepl("ENTERPRISE",TVM$Description_Fin),1,0)
TVM$ENTIREFLOORFLAG	                <-   	ifelse(grepl("ENTIRE FLOOR",TVM$Description_Fin),1,0)
TVM$ENTIRETEAMFLAG	                <-   	ifelse(grepl("ENTIRE TEAM",TVM$Description_Fin),1,0)
TVM$EP2FLAG	                <-   	ifelse(grepl("EP2",TVM$Description_Fin),1,0)
TVM$EPDMFLAG	                <-   	ifelse(grepl("EPDM",TVM$Description_Fin),1,0)
TVM$EPRESCRIBEFLAG	                <-   	ifelse(grepl("EPRESCRIBE",TVM$Description_Fin),1,0)
TVM$EQ1FLAG	                <-   	ifelse(grepl("EQ1",TVM$Description_Fin),1,0)
TVM$EQUIPMENTFLAG	                <-   	ifelse(grepl("EQUIPMENT",TVM$Description_Fin),1,0)
TVM$ERRORFLAG	                <-   	ifelse(grepl("ERROR",TVM$Description_Fin),1,0)
TVM$ESNAFLAG	                <-   	ifelse(grepl("ESNA",TVM$Description_Fin),1,0)
TVM$ESUBMITFLAG	                <-   	ifelse(grepl("ESUBMIT",TVM$Description_Fin),1,0)
TVM$EVENTFLAG	                <-   	ifelse(grepl("EVENT",TVM$Description_Fin),1,0)
TVM$EVENTMANAGERFLAG	                <-   	ifelse(grepl("EVENT MANAGER",TVM$Description_Fin),1,0)
TVM$EVERYBODYFLAG	                <-   	ifelse(grepl("EVERYBODY",TVM$Description_Fin),1,0)
TVM$EVOFLAG	                <-   	ifelse(grepl("EVO",TVM$Description_Fin),1,0)
TVM$EXCELFLAG	                <-   	ifelse(grepl("EXCEL",TVM$Description_Fin),1,0)
TVM$EXCHANGEFLAG	                <-   	ifelse(grepl("EXCHANGE",TVM$Description_Fin),1,0)
TVM$EXITCAREFLAG	                <-   	ifelse(grepl("EXIT CARE",TVM$Description_Fin),1,0)
TVM$EXITCAREFLAG2	                <-   	ifelse(grepl("EXITCARE",TVM$Description_Fin),1,0)
TVM$EXPENSFLAG	                <-   	ifelse(grepl("EXPENS",TVM$Description_Fin),1,0)
TVM$EXPIREDFLAG	                <-   	ifelse(grepl("EXPIRED",TVM$Description_Fin),1,0)
TVM$EXPIRFLAG	                <-   	ifelse(grepl("EXPIR",TVM$Description_Fin),1,0)
TVM$EXPORTFLAG	                <-   	ifelse(grepl("EXPORT",TVM$Description_Fin),1,0)
TVM$EXTENDFLAG	                <-   	ifelse(grepl("EXTEND",TVM$Description_Fin),1,0)
TVM$EXTRANETFLAG	                <-   	ifelse(grepl("EXTRANET",TVM$Description_Fin),1,0)
TVM$FABFLAG	                <-   	ifelse(grepl("FAB",TVM$Description_Fin),1,0)
TVM$FABMAINT2FLAG	                <-   	ifelse(grepl("FABMAINT2",TVM$Description_Fin),1,0)
TVM$FABRICFLAG	                <-   	ifelse(grepl("FABRIC",TVM$Description_Fin),1,0)
TVM$FACESHEETFLAG	                <-   	ifelse(grepl("FACESHEET",TVM$Description_Fin),1,0)
TVM$FACILITIESFLAG	                <-   	ifelse(grepl("FACILITIES",TVM$Description_Fin),1,0)
TVM$FAILEDFLAG	                <-   	ifelse(grepl("FAILED",TVM$Description_Fin),1,0)
TVM$FAILFLAG	                <-   	ifelse(grepl("FAIL",TVM$Description_Fin),1,0)
TVM$FAILUREFLAG	                <-   	ifelse(grepl("FAILURE",TVM$Description_Fin),1,0)
TVM$FAUXFLAG	                <-   	ifelse(grepl("FAUX",TVM$Description_Fin),1,0)
TVM$FAXFLAG	                <-   	ifelse(grepl("FAX",TVM$Description_Fin),1,0)
TVM$FEEDFLAG	                <-   	ifelse(grepl("FEED",TVM$Description_Fin),1,0)
TVM$FEWFLAG	                <-   	ifelse(grepl("FEW",TVM$Description_Fin),1,0)
TVM$FILEFLAG	                <-   	ifelse(grepl("FILE",TVM$Description_Fin),1,0)
TVM$FILEZILLAFLAG	                <-   	ifelse(grepl("FILEZILLA",TVM$Description_Fin),1,0)
TVM$FINDFLAG	                <-   	ifelse(grepl("FIND",TVM$Description_Fin),1,0)
TVM$FINISHFLAG	                <-   	ifelse(grepl("FINISH",TVM$Description_Fin),1,0)
TVM$FIREWALLFLAG	                <-   	ifelse(grepl("FIREWALL",TVM$Description_Fin),1,0)
TVM$FIRMFLAG	                <-   	ifelse(grepl("FIRM",TVM$Description_Fin),1,0)
TVM$FISFLAG	                <-   	ifelse(grepl("FIS",TVM$Description_Fin),1,0)
TVM$FIXFLAG	                <-   	ifelse(grepl("FIX",TVM$Description_Fin),1,0)
TVM$FLASHFLAG	                <-   	ifelse(grepl("FLASH",TVM$Description_Fin),1,0)
TVM$FLOWSHEETFLAG	                <-   	ifelse(grepl("FLOWSHEET",TVM$Description_Fin),1,0)
TVM$FLUCUATFLAG	                <-   	ifelse(grepl("FLUCUAT",TVM$Description_Fin),1,0)
TVM$FOLDERFLAG	                <-   	ifelse(grepl("FOLDER",TVM$Description_Fin),1,0)
TVM$FOLLOWFLAG	                <-   	ifelse(grepl("FOLLOW",TVM$Description_Fin),1,0)
TVM$FORGOTFLAG	                <-   	ifelse(grepl("FORGOT",TVM$Description_Fin),1,0)
TVM$FORMATFLAG	                <-   	ifelse(grepl("FORMAT",TVM$Description_Fin),1,0)
TVM$FORWARDFLAG	                <-   	ifelse(grepl("FORWARD",TVM$Description_Fin),1,0)
TVM$FOUNDFLAG	                <-   	ifelse(grepl("FOUND",TVM$Description_Fin),1,0)
TVM$FRAMEFLAG	                <-   	ifelse(grepl("FRAME",TVM$Description_Fin),1,0)
TVM$FREEFLAG	                <-   	ifelse(grepl("FREE",TVM$Description_Fin),1,0)
TVM$FREEZEDFLAG	                <-   	ifelse(grepl("FREEZED",TVM$Description_Fin),1,0)
TVM$FREEZESFLAG	                <-   	ifelse(grepl("FREEZES",TVM$Description_Fin),1,0)
TVM$FREEZESSFLAG	                <-   	ifelse(grepl("FREEZESS",TVM$Description_Fin),1,0)
TVM$FREEZFLAG	                <-   	ifelse(grepl("FREEZ",TVM$Description_Fin),1,0)
TVM$FREEZINGFLAG	                <-   	ifelse(grepl("FREEZING",TVM$Description_Fin),1,0)
TVM$FRONTIERFLAG	                <-   	ifelse(grepl("FRONTIER",TVM$Description_Fin),1,0)
TVM$FROZEFLAG	                <-   	ifelse(grepl("FROZE",TVM$Description_Fin),1,0)
TVM$FROZENFLAG	                <-   	ifelse(grepl("FROZEN",TVM$Description_Fin),1,0)
TVM$FROZFLAG	                <-   	ifelse(grepl("FROZ",TVM$Description_Fin),1,0)
TVM$FSFLAG	                <-   	ifelse(grepl("FS",TVM$Description_Fin),1,0)
TVM$FTPFLAG	                <-   	ifelse(grepl("FTP",TVM$Description_Fin),1,0)
TVM$FULLFLAG	                <-   	ifelse(grepl("FULL",TVM$Description_Fin),1,0)
TVM$FUNCTIONFLAG	                <-   	ifelse(grepl("FUNCTION",TVM$Description_Fin),1,0)
TVM$FUSERFLAG	                <-   	ifelse(grepl("FUSER",TVM$Description_Fin),1,0)
TVM$FUSIONOPSFLAG	                <-   	ifelse(grepl("FUSIONOPS",TVM$Description_Fin),1,0)
TVM$GAVSFLAG	                <-   	ifelse(grepl("GAVS",TVM$Description_Fin),1,0)
TVM$GBFLAG	                <-   	ifelse(grepl("GB",TVM$Description_Fin),1,0)
TVM$GEARFLAG	                <-   	ifelse(grepl("GEAR",TVM$Description_Fin),1,0)
TVM$GENERALFLAG	                <-   	ifelse(grepl("GENERAL",TVM$Description_Fin),1,0)
TVM$GERBERFLAG	                <-   	ifelse(grepl("GERBER",TVM$Description_Fin),1,0)
TVM$GETFLAG	                <-   	ifelse(grepl("GET",TVM$Description_Fin),1,0)
TVM$GETINFLAG	                <-   	ifelse(grepl("GET IN",TVM$Description_Fin),1,0)
TVM$GIVFLAG	                <-   	ifelse(grepl("GIV",TVM$Description_Fin),1,0)
TVM$GMAILFLAG	                <-   	ifelse(grepl("GMAIL",TVM$Description_Fin),1,0)
TVM$GOFLAG	                <-   	ifelse(grepl("GO",TVM$Description_Fin),1,0)
TVM$GOODSFLAG	                <-   	ifelse(grepl("GOODS",TVM$Description_Fin),1,0)
TVM$GOOGLEFLAG	                <-   	ifelse(grepl("GOOGLE",TVM$Description_Fin),1,0)
TVM$GRACEFLAG	                <-   	ifelse(grepl("GRACE",TVM$Description_Fin),1,0)
TVM$GROUPFLAG	                <-   	ifelse(grepl("GROUP",TVM$Description_Fin),1,0)
TVM$GROUPWISEFLAG	                <-   	ifelse(grepl("GROUPWISE",TVM$Description_Fin),1,0)
TVM$HACKFLAG	                <-   	ifelse(grepl("HACK",TVM$Description_Fin),1,0)
TVM$HALFFLAG	                <-   	ifelse(grepl("HALF",TVM$Description_Fin),1,0)
TVM$HARDDRIVEFLAG	                <-   	ifelse(grepl("HARD DRIVE",TVM$Description_Fin),1,0)
TVM$HARDFLAG	                <-   	ifelse(grepl("HARD",TVM$Description_Fin),1,0)
TVM$HARDWAREFLAG	                <-   	ifelse(grepl("HARDWARE",TVM$Description_Fin),1,0)
TVM$HDGUESTFLAG	                <-   	ifelse(grepl("HDGUEST",TVM$Description_Fin),1,0)
TVM$HDNAFLAG	                <-   	ifelse(grepl("HDNA",TVM$Description_Fin),1,0)
TVM$HEADSETFLAG	                <-   	ifelse(grepl("HEADSET",TVM$Description_Fin),1,0)
TVM$HEADSETFLAG2	                <-   	ifelse(grepl("HEAD SET",TVM$Description_Fin),1,0)
TVM$HEALTHFLAG	                <-   	ifelse(grepl("HEALTH",TVM$Description_Fin),1,0)
TVM$HELPFLAG	                <-   	ifelse(grepl("HELP",TVM$Description_Fin),1,0)
TVM$HELPDESKFLAG	                <-   	ifelse(grepl("HELP DESK",TVM$Description_Fin),1,0)
TVM$HELPDESKFLAG2	                <-   	ifelse(grepl("HELPDESK",TVM$Description_Fin),1,0)
TVM$HIDDENFLAG	                <-   	ifelse(grepl("HIDDEN",TVM$Description_Fin),1,0)
TVM$HIGHFLAG	                <-   	ifelse(grepl("HIGH",TVM$Description_Fin),1,0)
TVM$HINGINGFLAG	                <-   	ifelse(grepl("HINGING",TVM$Description_Fin),1,0)
TVM$HIREFLAG	                <-   	ifelse(grepl("HIRE",TVM$Description_Fin),1,0)
TVM$HOISTFLAG	                <-   	ifelse(grepl("HOIST",TVM$Description_Fin),1,0)
TVM$HOISTSFLAG	                <-   	ifelse(grepl("HOISTS",TVM$Description_Fin),1,0)
TVM$HORIZONFLAG	                <-   	ifelse(grepl("HORIZON",TVM$Description_Fin),1,0)
TVM$HOSTFLAG	                <-   	ifelse(grepl("HOST",TVM$Description_Fin),1,0)
TVM$HUNGFLAG	                <-   	ifelse(grepl("HUNG",TVM$Description_Fin),1,0)
TVM$IDFLAG	                <-   	ifelse(grepl("ID",TVM$Description_Fin),1,0)
TVM$IEFLAG	                <-   	ifelse(grepl("IE",TVM$Description_Fin),1,0)
TVM$IMAGECASTFLAG	                <-   	ifelse(grepl("IMAGECAST",TVM$Description_Fin),1,0)
TVM$IMAGECASTFLAG2	                <-   	ifelse(grepl("IMAGE CAST",TVM$Description_Fin),1,0)
TVM$IMAGEFLAG	                <-   	ifelse(grepl("IMAGE",TVM$Description_Fin),1,0)
TVM$IMPLEMENTFLAG	                <-   	ifelse(grepl("IMPLEMENT",TVM$Description_Fin),1,0)
TVM$INCIDENTFLAG	                <-   	ifelse(grepl("INCIDENT",TVM$Description_Fin),1,0)
TVM$INCORRECTFLAG	                <-   	ifelse(grepl("INCORRECT",TVM$Description_Fin),1,0)
TVM$INDIVIDUALSFLAG	                <-   	ifelse(grepl("INDIVIDUALS",TVM$Description_Fin),1,0)
TVM$INDUSOFTFLAG	                <-   	ifelse(grepl("INDUSOFT",TVM$Description_Fin),1,0)
TVM$INFECTEDFLAG	                <-   	ifelse(grepl("INFECTED",TVM$Description_Fin),1,0)
TVM$INFECTFLAG	                <-   	ifelse(grepl("INFECT",TVM$Description_Fin),1,0)
TVM$INFOFLAG	                <-   	ifelse(grepl("INFO",TVM$Description_Fin),1,0)
TVM$INFOREQUESTFLAG	                <-   	ifelse(grepl("INFOREQUEST",TVM$Description_Fin),1,0)
TVM$ININFLAG	                <-   	ifelse(grepl("ININ",TVM$Description_Fin),1,0)
TVM$INKFLAG	                <-   	ifelse(grepl("INK",TVM$Description_Fin),1,0)
TVM$INSPECTIONFLAG	                <-   	ifelse(grepl("INSPECTION",TVM$Description_Fin),1,0)
TVM$INSTALLFLAG	                <-   	ifelse(grepl("INSTALL",TVM$Description_Fin),1,0)
TVM$INSTEADFLAG	                <-   	ifelse(grepl("INSTEAD",TVM$Description_Fin),1,0)
TVM$INSURANCEFLAG	                <-   	ifelse(grepl("INSURANCE",TVM$Description_Fin),1,0)
TVM$INTELLISILLFLAG	                <-   	ifelse(grepl("INTELLISILL",TVM$Description_Fin),1,0)
TVM$INTERFACEFLAG	                <-   	ifelse(grepl("INTERFACE",TVM$Description_Fin),1,0)
TVM$INTERMITTENTFLAG	                <-   	ifelse(grepl("INTERMITTENT",TVM$Description_Fin),1,0)
TVM$INTERNETFLAG	                <-   	ifelse(grepl("INTERNET",TVM$Description_Fin),1,0)
TVM$INTERNFLAG	                <-   	ifelse(grepl("INTERN",TVM$Description_Fin),1,0)
TVM$INTERNALFLAG	                <-   	ifelse(grepl("INTERNAL",TVM$Description_Fin),1,0)
TVM$INTERNATIONALFLAG	                <-   	ifelse(grepl("INTERNATIONAL",TVM$Description_Fin),1,0)
TVM$INTERQUALFLAG	                <-   	ifelse(grepl("INTERQUAL",TVM$Description_Fin),1,0)
TVM$INTRANETFLAG	                <-   	ifelse(grepl("INTRANET",TVM$Description_Fin),1,0)
TVM$INVALIDFLAG	                <-   	ifelse(grepl("INVALID",TVM$Description_Fin),1,0)
TVM$INVENTORYFLAG	                <-   	ifelse(grepl("INVENTORY",TVM$Description_Fin),1,0)
TVM$INVERTFLAG	                <-   	ifelse(grepl("INVERT",TVM$Description_Fin),1,0)
TVM$INVOICEFLAG	                <-   	ifelse(grepl("INVOICE",TVM$Description_Fin),1,0)
TVM$INVOICESFLAG	                <-   	ifelse(grepl("INVOICES",TVM$Description_Fin),1,0)
TVM$IPADFLAG	                <-   	ifelse(grepl("IPAD",TVM$Description_Fin),1,0)
TVM$IPFLAG	                <-   	ifelse(grepl("IP",TVM$Description_Fin),1,0)
TVM$IPHONEFLAG	                <-   	ifelse(grepl("IPHONE",TVM$Description_Fin),1,0)
TVM$ISFLAG	                <-   	ifelse(grepl(" IS ",TVM$Description_Fin),1,0)
TVM$ISNTFLAG	                <-   	ifelse(grepl("ISNT",TVM$Description_Fin),1,0)
TVM$ISNTWORKFLAG	                <-   	ifelse(grepl("ISNT WORK",TVM$Description_Fin),1,0)
TVM$ISSFLAG	                <-   	ifelse(grepl("ISS",TVM$Description_Fin),1,0)
TVM$ISSUEDFLAG	                <-   	ifelse(grepl("ISSUED",TVM$Description_Fin),1,0)
TVM$ISSUEFLAG	                <-   	ifelse(grepl("ISSUE",TVM$Description_Fin),1,0)
TVM$ISSUESFLAG	                <-   	ifelse(grepl("ISSUES",TVM$Description_Fin),1,0)
TVM$ITCHECKFLAG	                <-   	ifelse(grepl("ITCHECK",TVM$Description_Fin),1,0)
TVM$ITEMFLAG	                <-   	ifelse(grepl("ITEM",TVM$Description_Fin),1,0)
TVM$ITSUPPORTFLAG	                <-   	ifelse(grepl("ITSUPPORT",TVM$Description_Fin),1,0)
TVM$ITSUPPORTFLAG2	                <-   	ifelse(grepl("IT SUPPORT",TVM$Description_Fin),1,0)
TVM$ITUNESFLAG	                <-   	ifelse(grepl("ITUNES",TVM$Description_Fin),1,0)
TVM$JAMFLAG	                <-   	ifelse(grepl("JAM",TVM$Description_Fin),1,0)
TVM$JAVAFLAG	                <-   	ifelse(grepl("JAVA",TVM$Description_Fin),1,0)
TVM$JINGFLAG	                <-   	ifelse(grepl("JING",TVM$Description_Fin),1,0)
TVM$JIRAFLAG	                <-   	ifelse(grepl("JIRA",TVM$Description_Fin),1,0)
TVM$JOBFLAG	                <-   	ifelse(grepl("JOB",TVM$Description_Fin),1,0)
TVM$JOBSFLAG	                <-   	ifelse(grepl("JOBS",TVM$Description_Fin),1,0)
TVM$JUNIPERFLAG	                <-   	ifelse(grepl("JUNIPER",TVM$Description_Fin),1,0)
TVM$JUNKFLAG	                <-   	ifelse(grepl("JUNK",TVM$Description_Fin),1,0)
TVM$KAIZENFLAG	                <-   	ifelse(grepl("KAIZEN",TVM$Description_Fin),1,0)
TVM$KEYBOARDFLAG	                <-   	ifelse(grepl("KEYBOARD",TVM$Description_Fin),1,0)
TVM$KICKEDOUTFLAG	                <-   	ifelse(grepl("KICKED OUT",TVM$Description_Fin),1,0)
TVM$KIOSKFLAG	                <-   	ifelse(grepl("KIOSK",TVM$Description_Fin),1,0)
TVM$KITACEFLAG	                <-   	ifelse(grepl("KIT ACE",TVM$Description_Fin),1,0)
TVM$KNOWFLAG	                <-   	ifelse(grepl("KNOW",TVM$Description_Fin),1,0)
TVM$KRONOSFLAG	                <-   	ifelse(grepl("KRONOS",TVM$Description_Fin),1,0)
TVM$LABELFLAG	                <-   	ifelse(grepl("LABEL",TVM$Description_Fin),1,0)
TVM$LABFLAG	                <-   	ifelse(grepl("LAB",TVM$Description_Fin),1,0)
TVM$LABLABELFLAG	                <-   	ifelse(grepl("LAB LABEL",TVM$Description_Fin),1,0)
TVM$LABLEFLAG	                <-   	ifelse(grepl("LABLE",TVM$Description_Fin),1,0)
TVM$LABLESFLAG	                <-   	ifelse(grepl("LABLES",TVM$Description_Fin),1,0)
TVM$LABREPORTFLAG	                <-   	ifelse(grepl("LAB REPORT",TVM$Description_Fin),1,0)
TVM$LABTESTFLAG	                <-   	ifelse(grepl("LAB TEST",TVM$Description_Fin),1,0)
TVM$LANFLAG	                <-   	ifelse(grepl("LAN",TVM$Description_Fin),1,0)
TVM$LAPTOPFLAG	                <-   	ifelse(grepl("LAPTOP",TVM$Description_Fin),1,0)
TVM$LASERFLAG	                <-   	ifelse(grepl("LASER",TVM$Description_Fin),1,0)
TVM$LATENCYFLAG	                <-   	ifelse(grepl("LATENCY",TVM$Description_Fin),1,0)
TVM$LATESTFLAG	                <-   	ifelse(grepl("LATEST",TVM$Description_Fin),1,0)
TVM$LAUNCHFLAG	                <-   	ifelse(grepl("LAUNCH",TVM$Description_Fin),1,0)
TVM$LAUNCHINGFLAG	                <-   	ifelse(grepl("LAUNCHING",TVM$Description_Fin),1,0)
TVM$LESSTHANFLAG	                <-   	ifelse(grepl("LESS THAN",TVM$Description_Fin),1,0)
TVM$LICENSFLAG	                <-   	ifelse(grepl("LICENS",TVM$Description_Fin),1,0)
TVM$LIMITFLAG	                <-   	ifelse(grepl("LIMIT",TVM$Description_Fin),1,0)
TVM$LINEFLAG	                <-   	ifelse(grepl("LINE",TVM$Description_Fin),1,0)
TVM$LINESFLAG	                <-   	ifelse(grepl("LINES",TVM$Description_Fin),1,0)
TVM$LINKFLAG	                <-   	ifelse(grepl("LINK",TVM$Description_Fin),1,0)
TVM$LINKAGFLAG	                <-   	ifelse(grepl("LINKAG",TVM$Description_Fin),1,0)
TVM$LINUXFLAG	                <-   	ifelse(grepl("LINUX",TVM$Description_Fin),1,0)
TVM$LISTENFLAG	                <-   	ifelse(grepl("LISTEN",TVM$Description_Fin),1,0)
TVM$LISTFLAG	                <-   	ifelse(grepl("LIST",TVM$Description_Fin),1,0)
TVM$LITEFLAG	                <-   	ifelse(grepl("LITE",TVM$Description_Fin),1,0)
TVM$LOADINGFLAG	                <-   	ifelse(grepl("LOADING",TVM$Description_Fin),1,0)
TVM$LOANFLAG	                <-   	ifelse(grepl("LOAN",TVM$Description_Fin),1,0)
TVM$LOCKFLAG	                <-   	ifelse(grepl("LOCK",TVM$Description_Fin),1,0)
TVM$LOGFLAG	                <-   	ifelse(grepl("LOG",TVM$Description_Fin),1,0)
TVM$LOGGINGFLAG	                <-   	ifelse(grepl("LOGGING",TVM$Description_Fin),1,0)
TVM$LOGINFLAG	                <-   	ifelse(grepl("LOGIN",TVM$Description_Fin),1,0)
TVM$LOGINFLAG2	                <-   	ifelse(grepl("LOG IN",TVM$Description_Fin),1,0)
TVM$LOGONFLAG	                <-   	ifelse(grepl("LOGON",TVM$Description_Fin),1,0)
TVM$LOGONFLAG2	                <-   	ifelse(grepl("LOG ON",TVM$Description_Fin),1,0)
TVM$LOGOUTFLAG	                <-   	ifelse(grepl("LOGOUT",TVM$Description_Fin),1,0)
TVM$LOGSOFFFLAG	                <-   	ifelse(grepl("LOGS OFF",TVM$Description_Fin),1,0)
TVM$LONGFLAG	                <-   	ifelse(grepl("LONG",TVM$Description_Fin),1,0)
TVM$LOOKFLAG	                <-   	ifelse(grepl("LOOK",TVM$Description_Fin),1,0)
TVM$LOOPFLAG	                <-   	ifelse(grepl("LOOP",TVM$Description_Fin),1,0)
TVM$LOOSFLAG	                <-   	ifelse(grepl("LOOS",TVM$Description_Fin),1,0)
TVM$LOSFLAG	                <-   	ifelse(grepl("LOS",TVM$Description_Fin),1,0)
TVM$LOSSFLAG	                <-   	ifelse(grepl("LOSS",TVM$Description_Fin),1,0)
TVM$LOSTFLAG	                <-   	ifelse(grepl("LOST",TVM$Description_Fin),1,0)
TVM$LOWQUALITYFLAG	                <-   	ifelse(grepl("LOW QUALITY",TVM$Description_Fin),1,0)
TVM$LYNCFLAG	                <-   	ifelse(grepl("LYNC",TVM$Description_Fin),1,0)
TVM$MACHINEFLAG	                <-   	ifelse(grepl("MACHINE",TVM$Description_Fin),1,0)
TVM$MAILFLAG	                <-   	ifelse(grepl("MAIL",TVM$Description_Fin),1,0)
TVM$MAINTENANCEFLAG	                <-   	ifelse(grepl("MAINTENANCE",TVM$Description_Fin),1,0)
TVM$MAKEFLAG	                <-   	ifelse(grepl("MAKE",TVM$Description_Fin),1,0)
TVM$MALFUNCTIONFLAG	                <-   	ifelse(grepl("MALFUNCTION",TVM$Description_Fin),1,0)
TVM$MALWAREFLAG	                <-   	ifelse(grepl("MALWARE",TVM$Description_Fin),1,0)
TVM$MANAGERFLAG	                <-   	ifelse(grepl("MANAGER",TVM$Description_Fin),1,0)
TVM$MANYUSERFLAG	                <-   	ifelse(grepl("MANY USER",TVM$Description_Fin),1,0)
TVM$MAPFLAG	                <-   	ifelse(grepl("MAP",TVM$Description_Fin),1,0)
TVM$MARKETFLAG	                <-   	ifelse(grepl("MARKET",TVM$Description_Fin),1,0)
TVM$MASTERTICKETFLAG	                <-   	ifelse(grepl("MASTER TICKET",TVM$Description_Fin),1,0)
TVM$MATFLAG	                <-   	ifelse(grepl("MAT",TVM$Description_Fin),1,0)
TVM$MATERIALFLAG	                <-   	ifelse(grepl("MATERIAL",TVM$Description_Fin),1,0)
TVM$MATERIALSFLAG	                <-   	ifelse(grepl("MATERIALS",TVM$Description_Fin),1,0)
TVM$MCAFEEFLAG	                <-   	ifelse(grepl("MCAFEE",TVM$Description_Fin),1,0)
TVM$MEDICATIONFLAG	                <-   	ifelse(grepl("MEDICATION",TVM$Description_Fin),1,0)
TVM$MEETFLAG	                <-   	ifelse(grepl("MEET",TVM$Description_Fin),1,0)
TVM$MEMBERFLAG	                <-   	ifelse(grepl("MEMBER",TVM$Description_Fin),1,0)
TVM$MEMORYFLAG	                <-   	ifelse(grepl("MEMORY",TVM$Description_Fin),1,0)
TVM$MENUFLAG	                <-   	ifelse(grepl("MENU",TVM$Description_Fin),1,0)
TVM$MESSAGEFLAG	                <-   	ifelse(grepl("MESSAGE",TVM$Description_Fin),1,0)
TVM$MICROSOFTFLAG	                <-   	ifelse(grepl("MICROSOFT",TVM$Description_Fin),1,0)
TVM$MIGRATFLAG	                <-   	ifelse(grepl("MIGRAT",TVM$Description_Fin),1,0)
TVM$MISALIGNEDFLAG	                <-   	ifelse(grepl("MISALIGNED",TVM$Description_Fin),1,0)
TVM$MISCSHIPFLAG	                <-   	ifelse(grepl("MISCSHIP",TVM$Description_Fin),1,0)
TVM$MISPRINTFLAG	                <-   	ifelse(grepl("MISPRINT",TVM$Description_Fin),1,0)
TVM$MISSFLAG	                <-   	ifelse(grepl("MISS",TVM$Description_Fin),1,0)
TVM$MISSINGFLAG	                <-   	ifelse(grepl("MISSING",TVM$Description_Fin),1,0)
TVM$MODIFYFLAG	                <-   	ifelse(grepl("MODIFY",TVM$Description_Fin),1,0)
TVM$MONITORFLAG	                <-   	ifelse(grepl("MONITOR",TVM$Description_Fin),1,0)
TVM$MONITORINGFLAG	                <-   	ifelse(grepl("MONITORING",TVM$Description_Fin),1,0)
TVM$MOUSEFLAG	                <-   	ifelse(grepl("MOUSE",TVM$Description_Fin),1,0)
TVM$MOVEFLAG	                <-   	ifelse(grepl("MOVE",TVM$Description_Fin),1,0)
TVM$MPLSFLAG	                <-   	ifelse(grepl("MPLS",TVM$Description_Fin),1,0)
TVM$MSOFFICEFLAG	                <-   	ifelse(grepl("MS OFFICE",TVM$Description_Fin),1,0)
TVM$MUSEFLAG	                <-   	ifelse(grepl("MUSE",TVM$Description_Fin),1,0)
TVM$MYCOMPUTERFLAG	                <-   	ifelse(grepl("MY COMPUTER",TVM$Description_Fin),1,0)
TVM$NEEDFLAG	                <-   	ifelse(grepl("NEED",TVM$Description_Fin),1,0)
TVM$NETFLAG	                <-   	ifelse(grepl("NET",TVM$Description_Fin),1,0)
TVM$NETWORKFLAG	                <-   	ifelse(grepl("NETWORK",TVM$Description_Fin),1,0)
TVM$NEWFLAG	                <-   	ifelse(grepl("NEW",TVM$Description_Fin),1,0)
TVM$NODEFLAG	                <-   	ifelse(grepl("NODE",TVM$Description_Fin),1,0)
TVM$NOFLAG	                <-   	ifelse(grepl("NO ",TVM$Description_Fin),1,0)
TVM$NOINFOFLAG	                <-   	ifelse(grepl("NO INFO",TVM$Description_Fin),1,0)
TVM$NOINTERNETFLAG	                <-   	ifelse(grepl("NO INTERNET",TVM$Description_Fin),1,0)
TVM$NONEOFTHEUSERFLAG	                <-   	ifelse(grepl("NONE OF THE USER",TVM$Description_Fin),1,0)
TVM$NONFLAG	                <-   	ifelse(grepl("NON",TVM$Description_Fin),1,0)
TVM$NONOFTHEUSERFLAG	                <-   	ifelse(grepl("NON OF THE USER",TVM$Description_Fin),1,0)
TVM$NOSHOWFLAG	                <-   	ifelse(grepl("NO SHOW",TVM$Description_Fin),1,0)
TVM$NOTABLEFLAG	                <-   	ifelse(grepl("NOT ABLE",TVM$Description_Fin),1,0)
TVM$NOTACCEPTFLAG	                <-   	ifelse(grepl("NOT ACCEPT",TVM$Description_Fin),1,0)
TVM$NOTACCESSFLAG	                <-   	ifelse(grepl("NOT ACCESS",TVM$Description_Fin),1,0)
TVM$NOTALLOWFLAG	                <-   	ifelse(grepl("NOT ALLOW",TVM$Description_Fin),1,0)
TVM$NOTAUTHENTICATEFLAG	                <-   	ifelse(grepl("NOT AUTHENTICATE",TVM$Description_Fin),1,0)
TVM$NOTAVAILABLEFLAG	                <-   	ifelse(grepl("NOT AVAILABLE",TVM$Description_Fin),1,0)
TVM$NOTBACKFLAG	                <-   	ifelse(grepl("NOT BACK",TVM$Description_Fin),1,0)
TVM$NOTBEENCOMINGFLAG	                <-   	ifelse(grepl("NOT BEEN COMING",TVM$Description_Fin),1,0)
TVM$NOTCHARGFLAG	                <-   	ifelse(grepl("NOT CHARG",TVM$Description_Fin),1,0)
TVM$NOTCOMINGUPFLAG	                <-   	ifelse(grepl("NOT COMING UP",TVM$Description_Fin),1,0)
TVM$NOTCOMMUNICATINGFLAG	                <-   	ifelse(grepl("NOT COMMUNICATING",TVM$Description_Fin),1,0)
TVM$NOTCONNECTFLAG	                <-   	ifelse(grepl("NOT CONNECT",TVM$Description_Fin),1,0)
TVM$NOTCUTFLAG	                <-   	ifelse(grepl("NOT CUT",TVM$Description_Fin),1,0)
TVM$NOTESFLAG	                <-   	ifelse(grepl("NOTES",TVM$Description_Fin),1,0)
TVM$NOTEXCEPTFLAG	                <-   	ifelse(grepl("NOT EXCEPT",TVM$Description_Fin),1,0)
TVM$NOTFLAG	                <-   	ifelse(grepl("NOT ",TVM$Description_Fin),1,0)
TVM$NOTFOUNDFLAG	                <-   	ifelse(grepl("NOT FOUND",TVM$Description_Fin),1,0)
TVM$NOTFUNCTIONFLAG	                <-   	ifelse(grepl("NOT FUNCTION",TVM$Description_Fin),1,0)
TVM$NOTGETFLAG	                <-   	ifelse(grepl("NOT GET",TVM$Description_Fin),1,0)
TVM$NOTGIVFLAG	                <-   	ifelse(grepl("NOT GIV",TVM$Description_Fin),1,0)
TVM$NOTGOINGFLAG	                <-   	ifelse(grepl("NOT GOING",TVM$Description_Fin),1,0)
TVM$NOTICEFLAG	                <-   	ifelse(grepl("NOTICE",TVM$Description_Fin),1,0)
TVM$NOTIFFLAG	                <-   	ifelse(grepl("NOTIF",TVM$Description_Fin),1,0)
TVM$NOTIFICATIONFLAG	                <-   	ifelse(grepl("NOTIFICATION",TVM$Description_Fin),1,0)
TVM$NOTKEEPFLAG	                <-   	ifelse(grepl("NOT KEEP",TVM$Description_Fin),1,0)
TVM$NOTPICKUPFLAG	                <-   	ifelse(grepl("NOT PICK UP",TVM$Description_Fin),1,0)
TVM$NOTPRINTFLAG	                <-   	ifelse(grepl("NOT PRINT",TVM$Description_Fin),1,0)
TVM$NOTRECEIVFLAG	                <-   	ifelse(grepl("NOT RECEIV",TVM$Description_Fin),1,0)
TVM$NOTRESPONDFLAG	                <-   	ifelse(grepl("NOT RESPOND",TVM$Description_Fin),1,0)
TVM$NOTRESPONDINGFLAG	                <-   	ifelse(grepl("NOT RESPONDING",TVM$Description_Fin),1,0)
TVM$NOTSCANFLAG	                <-   	ifelse(grepl("NOT SCAN",TVM$Description_Fin),1,0)
TVM$NOTSHOWFLAG	                <-   	ifelse(grepl("NOT SHOW",TVM$Description_Fin),1,0)
TVM$NOTSTARTFLAG	                <-   	ifelse(grepl("NOT START",TVM$Description_Fin),1,0)
TVM$NOTTRANSFERFLAG	                <-   	ifelse(grepl("NOT TRANSFER",TVM$Description_Fin),1,0)
TVM$NOTUPDATFLAG	                <-   	ifelse(grepl("NOT UPDAT",TVM$Description_Fin),1,0)
TVM$NOTWORKFLAG	                <-   	ifelse(grepl("NOT WORK",TVM$Description_Fin),1,0)
TVM$NOTWORKINGFLAG	                <-   	ifelse(grepl("NOT WORKING",TVM$Description_Fin),1,0)
TVM$NOVELLFLAG	                <-   	ifelse(grepl("NOVELL",TVM$Description_Fin),1,0)
TVM$OFFCENTERFLAG	                <-   	ifelse(grepl("OFF CENTER",TVM$Description_Fin),1,0)
TVM$OFFFLAG	                <-   	ifelse(grepl("OFF",TVM$Description_Fin),1,0)
TVM$OFFICEFLAG	                <-   	ifelse(grepl("OFFICE",TVM$Description_Fin),1,0)
TVM$OFFLINEFLAG	                <-   	ifelse(grepl("OFFLINE",TVM$Description_Fin),1,0)
TVM$OFFLINEFLAG2	                <-   	ifelse(grepl("OFF LINE",TVM$Description_Fin),1,0)
TVM$ONLINEFLAG	                <-   	ifelse(grepl("ONLINE",TVM$Description_Fin),1,0)
TVM$OPENFLAG	                <-   	ifelse(grepl("OPEN",TVM$Description_Fin),1,0)
TVM$OPERATINGSYSTEMFLAG	                <-   	ifelse(grepl("OPERATING SYSTEM",TVM$Description_Fin),1,0)
TVM$ORACLEFLAG	                <-   	ifelse(grepl("ORACLE",TVM$Description_Fin),1,0)
TVM$ORDERFLAG	                <-   	ifelse(grepl("ORDER",TVM$Description_Fin),1,0)
TVM$ORDERSFLAG	                <-   	ifelse(grepl("ORDERS",TVM$Description_Fin),1,0)
TVM$OSFLAG	                <-   	ifelse(grepl("OS",TVM$Description_Fin),1,0)
TVM$OUTAGEFLAG	                <-   	ifelse(grepl("OUTAGE",TVM$Description_Fin),1,0)
TVM$OUTDATEDFLAG	                <-   	ifelse(grepl("OUTDATED",TVM$Description_Fin),1,0)
TVM$OUTFLAG	                <-   	ifelse(grepl("OUT",TVM$Description_Fin),1,0)
TVM$OUTLOOKFLAG	                <-   	ifelse(grepl("OUTLOOK",TVM$Description_Fin),1,0)
TVM$OUTPUTFLAG	                <-   	ifelse(grepl("OUTPUT",TVM$Description_Fin),1,0)
TVM$OUTSIDEFLAG	                <-   	ifelse(grepl("OUTSIDE",TVM$Description_Fin),1,0)
TVM$OVERFLAG	                <-   	ifelse(grepl("OVER",TVM$Description_Fin),1,0)
TVM$OWNFLAG	                <-   	ifelse(grepl("OWN",TVM$Description_Fin),1,0)
TVM$OZFLAG	                <-   	ifelse(grepl("OZ",TVM$Description_Fin),1,0)
TVM$PACKETFLAG	                <-   	ifelse(grepl("PACKET",TVM$Description_Fin),1,0)
TVM$PACKFLAG	                <-   	ifelse(grepl("PACK",TVM$Description_Fin),1,0)
TVM$PACKINGLISTFLAG	                <-   	ifelse(grepl("PACKING LIST",TVM$Description_Fin),1,0)
TVM$PACKINGSLIPFLAG	                <-   	ifelse(grepl("PACKING SLIP",TVM$Description_Fin),1,0)
TVM$PACKLISTFLAG	                <-   	ifelse(grepl("PACK LIST",TVM$Description_Fin),1,0)
TVM$PAFLAG	                <-   	ifelse(grepl("PA",TVM$Description_Fin),1,0)
TVM$PAGEFLAG	                <-   	ifelse(grepl("PAGE",TVM$Description_Fin),1,0)
TVM$PARTFLAG	                <-   	ifelse(grepl("PART",TVM$Description_Fin),1,0)
TVM$PARTSFLAG	                <-   	ifelse(grepl("PARTS",TVM$Description_Fin),1,0)
TVM$PASSWORDFLAG	                <-   	ifelse(grepl("PASSWORD",TVM$Description_Fin),1,0)
TVM$PATCHFLAG	                <-   	ifelse(grepl("PATCH",TVM$Description_Fin),1,0)
TVM$PATCOMFLAG	                <-   	ifelse(grepl("PATCOM",TVM$Description_Fin),1,0)
TVM$PATEINTFLAG	                <-   	ifelse(grepl("PATEINT",TVM$Description_Fin),1,0)
TVM$PATHFLAG	                <-   	ifelse(grepl("PATH",TVM$Description_Fin),1,0)
TVM$PATIENTFLAG	                <-   	ifelse(grepl("PATIENT",TVM$Description_Fin),1,0)
TVM$PAYFLAG	                <-   	ifelse(grepl("PAY",TVM$Description_Fin),1,0)
TVM$PAYROLLFLAG	                <-   	ifelse(grepl("PAYROLL",TVM$Description_Fin),1,0)
TVM$PCFLAG	                <-   	ifelse(grepl("PC",TVM$Description_Fin),1,0)
TVM$PCARDFLAG	                <-   	ifelse(grepl("PCARD",TVM$Description_Fin),1,0)
TVM$PCSFLAG	                <-   	ifelse(grepl("PCS",TVM$Description_Fin),1,0)
TVM$PDFFLAG	                <-   	ifelse(grepl("PDF",TVM$Description_Fin),1,0)
TVM$PEDOMETERFLAG	                <-   	ifelse(grepl("PEDOMETER",TVM$Description_Fin),1,0)
TVM$PERFORMANCEFLAG	                <-   	ifelse(grepl("PERFORMANCE",TVM$Description_Fin),1,0)
TVM$PERFORMINGFLAG	                <-   	ifelse(grepl("PERFORMING",TVM$Description_Fin),1,0)
TVM$PERMISSIONFLAG	                <-   	ifelse(grepl("PERMISSION",TVM$Description_Fin),1,0)
TVM$PHONEFLAG	                <-   	ifelse(grepl("PHONE",TVM$Description_Fin),1,0)
TVM$PHONEQUEUEFLAG	                <-   	ifelse(grepl("PHONE QUEUE",TVM$Description_Fin),1,0)
TVM$PHONESFLAG	                <-   	ifelse(grepl("PHONES",TVM$Description_Fin),1,0)
TVM$PICKFLAG	                <-   	ifelse(grepl("PICK",TVM$Description_Fin),1,0)
TVM$PLACEFLAG	                <-   	ifelse(grepl("PLACE",TVM$Description_Fin),1,0)
TVM$PLATEFLAG	                <-   	ifelse(grepl("PLATE",TVM$Description_Fin),1,0)
TVM$PLYMOUTHFLAG	                <-   	ifelse(grepl("PLYMOUTH",TVM$Description_Fin),1,0)
TVM$PLMFLAG	                <-   	ifelse(grepl("PLM",TVM$Description_Fin),1,0)
TVM$POFLAG	                <-   	ifelse(grepl("PO",TVM$Description_Fin),1,0)
TVM$POORFLAG	                <-   	ifelse(grepl("POOR",TVM$Description_Fin),1,0)
TVM$POORQUALITYFLAG	                <-   	ifelse(grepl("POOR QUALITY",TVM$Description_Fin),1,0)
TVM$PORTFLAG	                <-   	ifelse(grepl("PORT",TVM$Description_Fin),1,0)
TVM$PORTABLEFLAG	                <-   	ifelse(grepl("PORTABLE",TVM$Description_Fin),1,0)
TVM$PORTALFLAG	                <-   	ifelse(grepl("PORTAL",TVM$Description_Fin),1,0)
TVM$PORTSFLAG	                <-   	ifelse(grepl("PORTS",TVM$Description_Fin),1,0)
TVM$POSITIONFLAG	                <-   	ifelse(grepl("POSITION",TVM$Description_Fin),1,0)
TVM$POWERFLAG	                <-   	ifelse(grepl("POWER",TVM$Description_Fin),1,0)
TVM$POWERINGONFLAG	                <-   	ifelse(grepl("POWERING ON",TVM$Description_Fin),1,0)
TVM$PRESCRIBEFLAG	                <-   	ifelse(grepl("PRESCRIBE",TVM$Description_Fin),1,0)
TVM$PRESCRIPTIONFLAG	                <-   	ifelse(grepl("PRESCRIPTION",TVM$Description_Fin),1,0)
TVM$PREVILEGEFLAG	                <-   	ifelse(grepl("PREVILEGE",TVM$Description_Fin),1,0)
TVM$PRIMARYFLAG	                <-   	ifelse(grepl("PRIMARY",TVM$Description_Fin),1,0)
TVM$PRINTEDFLAG	                <-   	ifelse(grepl("PRINTED",TVM$Description_Fin),1,0)
TVM$PRINTERFLAG	                <-   	ifelse(grepl("PRINTER",TVM$Description_Fin),1,0)
TVM$PRINTFLAG	                <-   	ifelse(grepl("PRINT",TVM$Description_Fin),1,0)
TVM$PRINTINGFLAG	                <-   	ifelse(grepl("PRINTING",TVM$Description_Fin),1,0)
TVM$PRIORITYFLAG	                <-   	ifelse(grepl("PRIORITY",TVM$Description_Fin),1,0)
TVM$PRISCRIPTIONSFLAG	                <-   	ifelse(grepl("PRISCRIPTIONS",TVM$Description_Fin),1,0)
TVM$PRIVILEDGESFLAG	                <-   	ifelse(grepl("PRIVILEDGES",TVM$Description_Fin),1,0)
TVM$PRIVILEGEFLAG	                <-   	ifelse(grepl("PRIVILEGE",TVM$Description_Fin),1,0)
TVM$PRIVILFLAG	                <-   	ifelse(grepl("PRIVIL",TVM$Description_Fin),1,0)
TVM$PROBLEMFLAG	                <-   	ifelse(grepl("PROBLEM",TVM$Description_Fin),1,0)
TVM$PROBLEMSFLAG	                <-   	ifelse(grepl("PROBLEMS",TVM$Description_Fin),1,0)
TVM$PROCESSFLAG	                <-   	ifelse(grepl("PROCESS",TVM$Description_Fin),1,0)
TVM$PRODUCTFLAG	                <-   	ifelse(grepl("PRODUCT",TVM$Description_Fin),1,0)
TVM$PRODUCTIONFLAG	                <-   	ifelse(grepl("PRODUCTION",TVM$Description_Fin),1,0)
TVM$PROGRAMFLAG	                <-   	ifelse(grepl("PROGRAM",TVM$Description_Fin),1,0)
TVM$PROJECTFLAG	                <-   	ifelse(grepl("PROJECT",TVM$Description_Fin),1,0)
TVM$PROVATIONFLAG	                <-   	ifelse(grepl("PROVATION",TVM$Description_Fin),1,0)
TVM$PTOFLAG	                <-   	ifelse(grepl("PTO",TVM$Description_Fin),1,0)
TVM$PTOOLFLAG	                <-   	ifelse(grepl("PTOOL",TVM$Description_Fin),1,0)
TVM$PTSFLAG	                <-   	ifelse(grepl("PTS",TVM$Description_Fin),1,0)
TVM$PULLFLAG	                <-   	ifelse(grepl("PULL",TVM$Description_Fin),1,0)
TVM$PULLUPFLAG	                <-   	ifelse(grepl("PULL UP",TVM$Description_Fin),1,0)
TVM$PUNCHFLAG	                <-   	ifelse(grepl("PUNCH",TVM$Description_Fin),1,0)
TVM$PURCHFLAG	                <-   	ifelse(grepl("PURCH",TVM$Description_Fin),1,0)
TVM$PWFLAG	                <-   	ifelse(grepl("PW",TVM$Description_Fin),1,0)
TVM$QUEUEFLAG	                <-   	ifelse(grepl("QUEUE",TVM$Description_Fin),1,0)
TVM$QUICKFLAG	                <-   	ifelse(grepl("QUICK",TVM$Description_Fin),1,0)
TVM$QUITWORKFLAG	                <-   	ifelse(grepl("QUIT WORK",TVM$Description_Fin),1,0)
TVM$QUOTEFLAG	                <-   	ifelse(grepl("QUOTE",TVM$Description_Fin),1,0)
TVM$RAILFLAG	                <-   	ifelse(grepl("RAIL",TVM$Description_Fin),1,0)
TVM$RAILSAWFLAG	                <-   	ifelse(grepl("RAIL SAW",TVM$Description_Fin),1,0)
TVM$RAMFLAG	                <-   	ifelse(grepl("RAM",TVM$Description_Fin),1,0)
TVM$REACTIVATEFLAG	                <-   	ifelse(grepl("REACTIVATE",TVM$Description_Fin),1,0)
TVM$READSOFTFLAG	                <-   	ifelse(grepl("READSOFT",TVM$Description_Fin),1,0)
TVM$REBOOTFLAG	                <-   	ifelse(grepl("REBOOT",TVM$Description_Fin),1,0)
TVM$RECEIVEFLAG	                <-   	ifelse(grepl("RECEIVE",TVM$Description_Fin),1,0)
TVM$RECEIVERFLAG	                <-   	ifelse(grepl("RECEIVER",TVM$Description_Fin),1,0)
TVM$RECEIVFLAG	                <-   	ifelse(grepl("RECEIV",TVM$Description_Fin),1,0)
TVM$RECORDFLAG	                <-   	ifelse(grepl("RECORD",TVM$Description_Fin),1,0)
TVM$RECRUITFLAG	                <-   	ifelse(grepl("RECRUIT",TVM$Description_Fin),1,0)
TVM$REDUNDANCYFLAG	                <-   	ifelse(grepl("REDUNDANCY",TVM$Description_Fin),1,0)
TVM$REDUNDANTFLAG	                <-   	ifelse(grepl("REDUNDANT",TVM$Description_Fin),1,0)
TVM$REDUNDANTLINKFLAG	                <-   	ifelse(grepl("REDUNDANT LINK",TVM$Description_Fin),1,0)
TVM$REFUFLAG	                <-   	ifelse(grepl("REFU",TVM$Description_Fin),1,0)
TVM$REFUSOLFLAG	                <-   	ifelse(grepl("REFUSOL",TVM$Description_Fin),1,0)
TVM$REGFLAG	                <-   	ifelse(grepl("REG ",TVM$Description_Fin),1,0)
TVM$REGISTERFLAG	                <-   	ifelse(grepl("REGISTER",TVM$Description_Fin),1,0)
TVM$REGISTRATIONFLAG	                <-   	ifelse(grepl("REGISTRATION",TVM$Description_Fin),1,0)
TVM$REGLABELFLAG	                <-   	ifelse(grepl("REG LABEL",TVM$Description_Fin),1,0)
TVM$RELEASEFLAG	                <-   	ifelse(grepl("RELEASE",TVM$Description_Fin),1,0)
TVM$REMINDFLAG	                <-   	ifelse(grepl("REMIND",TVM$Description_Fin),1,0)
TVM$REMOTEDESKTOPFLAG	                <-   	ifelse(grepl("REMOTE DESKTOP",TVM$Description_Fin),1,0)
TVM$REMOTEFLAG	                <-   	ifelse(grepl("REMOTE",TVM$Description_Fin),1,0)
TVM$REMOVFLAG	                <-   	ifelse(grepl("REMOV",TVM$Description_Fin),1,0)
TVM$REMOVEFLAG	                <-   	ifelse(grepl("REMOVE",TVM$Description_Fin),1,0)
TVM$RENEWFLAG	                <-   	ifelse(grepl("RENEW",TVM$Description_Fin),1,0)
TVM$REPAIRFLAG	                <-   	ifelse(grepl("REPAIR",TVM$Description_Fin),1,0)
TVM$REPLACEFLAG	                <-   	ifelse(grepl("REPLACE",TVM$Description_Fin),1,0)
TVM$REPORTFLAG	                <-   	ifelse(grepl("REPORT",TVM$Description_Fin),1,0)
TVM$REPORTINGFLAG	                <-   	ifelse(grepl("REPORTING",TVM$Description_Fin),1,0)
TVM$REPUBLICFLAG	                <-   	ifelse(grepl("REPUBLIC",TVM$Description_Fin),1,0)
TVM$REQFLAG	                <-   	ifelse(grepl("REQ",TVM$Description_Fin),1,0)
TVM$REQUESTFLAG	                <-   	ifelse(grepl("REQUEST",TVM$Description_Fin),1,0)
TVM$REQUIREDFLAG	                <-   	ifelse(grepl("REQUIRED",TVM$Description_Fin),1,0)
TVM$RESETFLAG	                <-   	ifelse(grepl("RESET",TVM$Description_Fin),1,0)
TVM$RESETFLAG2	                <-   	ifelse(grepl("RE-SET",TVM$Description_Fin),1,0)
TVM$RESOURCEFLAG	                <-   	ifelse(grepl("RESOURCE",TVM$Description_Fin),1,0)
TVM$RESPONDFLAG	                <-   	ifelse(grepl("RESPOND",TVM$Description_Fin),1,0)
TVM$RESTARTEDFLAG	                <-   	ifelse(grepl("RESTARTED",TVM$Description_Fin),1,0)
TVM$RESTARTFLAG	                <-   	ifelse(grepl("RESTART",TVM$Description_Fin),1,0)
TVM$RESTARTINGFLAG	                <-   	ifelse(grepl("RESTARTING",TVM$Description_Fin),1,0)
TVM$RESTORFLAG	                <-   	ifelse(grepl("RESTOR",TVM$Description_Fin),1,0)
TVM$RESULTSFLAG	                <-   	ifelse(grepl("RESULTS",TVM$Description_Fin),1,0)
TVM$RIBBONFLAG	                <-   	ifelse(grepl("RIBBON",TVM$Description_Fin),1,0)
TVM$RIGHTFLAG	                <-   	ifelse(grepl("RIGHT",TVM$Description_Fin),1,0)
TVM$RIGHTSFLAG	                <-   	ifelse(grepl("RIGHTS",TVM$Description_Fin),1,0)
TVM$ROLEFLAG	                <-   	ifelse(grepl("ROLE",TVM$Description_Fin),1,0)
TVM$ROLLERFLAG	                <-   	ifelse(grepl("ROLLER",TVM$Description_Fin),1,0)
TVM$ROLLFLAG	                <-   	ifelse(grepl("ROLL",TVM$Description_Fin),1,0)
TVM$ROUTEFLAG	                <-   	ifelse(grepl("ROUTE",TVM$Description_Fin),1,0)
TVM$ROUTERFLAG	                <-   	ifelse(grepl("ROUTER",TVM$Description_Fin),1,0)
TVM$RUBYFLAG	                <-   	ifelse(grepl("RUBY",TVM$Description_Fin),1,0)
TVM$RUNFLAG	                <-   	ifelse(grepl("RUN",TVM$Description_Fin),1,0)
TVM$RUNTIMEFLAG	                <-   	ifelse(grepl("RUN TIME",TVM$Description_Fin),1,0)
TVM$SALEFLAG	                <-   	ifelse(grepl("SALE",TVM$Description_Fin),1,0)
TVM$SALESFORCEFLAG	                <-   	ifelse(grepl("SALESFORCE",TVM$Description_Fin),1,0)
TVM$SAMBAFLAG	                <-   	ifelse(grepl("SAMBA",TVM$Description_Fin),1,0)
TVM$SAPFLAG	                <-   	ifelse(grepl("SAP",TVM$Description_Fin),1,0)
TVM$SATATIONFLAG	                <-   	ifelse(grepl("SATATION",TVM$Description_Fin),1,0)
TVM$SAVEFLAG	                <-   	ifelse(grepl("SAVE",TVM$Description_Fin),1,0)
TVM$SAVFLAG	                <-   	ifelse(grepl("SAV",TVM$Description_Fin),1,0)
TVM$SAWFLAG	                <-   	ifelse(grepl("SAW",TVM$Description_Fin),1,0)
TVM$SCANFLAG	                <-   	ifelse(grepl("SCAN",TVM$Description_Fin),1,0)
TVM$SCANNERFLAG	                <-   	ifelse(grepl("SCANNER",TVM$Description_Fin),1,0)
TVM$SCHEDULEFLAG	                <-   	ifelse(grepl("SCHEDULE",TVM$Description_Fin),1,0)
TVM$SCHEDULFLAG	                <-   	ifelse(grepl("SCHEDUL",TVM$Description_Fin),1,0)
TVM$SCHEDULINGFLAG	                <-   	ifelse(grepl("SCHEDULING",TVM$Description_Fin),1,0)
TVM$SCOREBOARDFLAG	                <-   	ifelse(grepl("SCOREBOARD",TVM$Description_Fin),1,0)
TVM$SCRAPFLAG	                <-   	ifelse(grepl("SCRAP",TVM$Description_Fin),1,0)
TVM$SCREENFLAG	                <-   	ifelse(grepl("SCREEN",TVM$Description_Fin),1,0)
TVM$SEARCHFLAG	                <-   	ifelse(grepl("SEARCH",TVM$Description_Fin),1,0)
TVM$SEATFLAG	                <-   	ifelse(grepl("SEAT",TVM$Description_Fin),1,0)
TVM$SECURITYFLAG	                <-   	ifelse(grepl("SECURITY",TVM$Description_Fin),1,0)
TVM$SENDFLAG	                <-   	ifelse(grepl("SEND",TVM$Description_Fin),1,0)
TVM$SENSORFLAG	                <-   	ifelse(grepl("SENSOR",TVM$Description_Fin),1,0)
TVM$SERVERFLAG	                <-   	ifelse(grepl("SERVER",TVM$Description_Fin),1,0)
TVM$SERVICEDESKFLAG	                <-   	ifelse(grepl("SERVICEDESK",TVM$Description_Fin),1,0)
TVM$SERVICEFLAG	                <-   	ifelse(grepl("SERVICE",TVM$Description_Fin),1,0)
TVM$SETFLAG	                <-   	ifelse(grepl("SET",TVM$Description_Fin),1,0)
TVM$SETTINGFLAG	                <-   	ifelse(grepl("SETTING",TVM$Description_Fin),1,0)
TVM$SETUPFLAG	                <-   	ifelse(grepl("SETUP",TVM$Description_Fin),1,0)
TVM$SETUPFLAG2	                <-   	ifelse(grepl("SET UP",TVM$Description_Fin),1,0)
TVM$SEVERALFLAG	                <-   	ifelse(grepl("SEVERAL",TVM$Description_Fin),1,0)
TVM$SFDCFLAG	                <-   	ifelse(grepl("SFDC",TVM$Description_Fin),1,0)
TVM$SHAREFLAG	                <-   	ifelse(grepl("SHARE",TVM$Description_Fin),1,0)
TVM$SHAREPOINTFLAG	                <-   	ifelse(grepl("SHAREPOINT",TVM$Description_Fin),1,0)
TVM$SHEETFLAG	                <-   	ifelse(grepl("SHEET",TVM$Description_Fin),1,0)
TVM$SHIPFLAG	                <-   	ifelse(grepl("SHIP",TVM$Description_Fin),1,0)
TVM$SHMFLAG	                <-   	ifelse(grepl("SHM",TVM$Description_Fin),1,0)
TVM$SHOWFLAG	                <-   	ifelse(grepl("SHOW",TVM$Description_Fin),1,0)
TVM$SHOWINGFLAG	                <-   	ifelse(grepl("SHOWING",TVM$Description_Fin),1,0)
TVM$SHUTFLAG	                <-   	ifelse(grepl("SHUT",TVM$Description_Fin),1,0)
TVM$SHUTSDOWNFLAG	                <-   	ifelse(grepl("SHUTS DOWN",TVM$Description_Fin),1,0)
TVM$SHUTSOFFFLAG	                <-   	ifelse(grepl("SHUTS OFF",TVM$Description_Fin),1,0)
TVM$SHUTTERSFLAG	                <-   	ifelse(grepl("SHUTTERS",TVM$Description_Fin),1,0)
TVM$SHUTTINGDOWNFLAG	                <-   	ifelse(grepl("SHUTTING DOWN",TVM$Description_Fin),1,0)
TVM$SIDEWAYSFLAG	                <-   	ifelse(grepl("SIDE WAYS",TVM$Description_Fin),1,0)
TVM$SIGNALFLAG	                <-   	ifelse(grepl("SIGNAL",TVM$Description_Fin),1,0)
TVM$SILHOUETTEFLAG	                <-   	ifelse(grepl("SILHOUETTE",TVM$Description_Fin),1,0)
TVM$SILKFLAG	                <-   	ifelse(grepl("SILK",TVM$Description_Fin),1,0)
TVM$SITEFLAG	                <-   	ifelse(grepl("SITE",TVM$Description_Fin),1,0)
TVM$SITESFLAG	                <-   	ifelse(grepl("SITES",TVM$Description_Fin),1,0)
TVM$SKYPEFLAG	                <-   	ifelse(grepl("SKYPE",TVM$Description_Fin),1,0)
TVM$SLIPFLAG	                <-   	ifelse(grepl("SLIP",TVM$Description_Fin),1,0)
TVM$SLOWFLAG	                <-   	ifelse(grepl("SLOW",TVM$Description_Fin),1,0)
TVM$SLOWLYFLAG	                <-   	ifelse(grepl("SLOWLY",TVM$Description_Fin),1,0)
TVM$SNAPFLAG	                <-   	ifelse(grepl("SNAP",TVM$Description_Fin),1,0)
TVM$SNCFLAG	                <-   	ifelse(grepl("SNC",TVM$Description_Fin),1,0)
TVM$SNMFLAG	                <-   	ifelse(grepl("SNM",TVM$Description_Fin),1,0)
TVM$SOFTWAREFLAG	                <-   	ifelse(grepl("SOFTWARE",TVM$Description_Fin),1,0)
TVM$SOLIDWORKFLAG	                <-   	ifelse(grepl("SOLIDWORK",TVM$Description_Fin),1,0)
TVM$SOMEONEFLAG	                <-   	ifelse(grepl("SOMEONE",TVM$Description_Fin),1,0)
TVM$SORTFLAG	                <-   	ifelse(grepl("SORT",TVM$Description_Fin),1,0)
TVM$SPACEFLAG	                <-   	ifelse(grepl("SPACE",TVM$Description_Fin),1,0)
TVM$SPAMFLAG	                <-   	ifelse(grepl("SPAM",TVM$Description_Fin),1,0)
TVM$SPYWAREFLAG	                <-   	ifelse(grepl("SPYWARE",TVM$Description_Fin),1,0)
TVM$SQFLAG	                <-   	ifelse(grepl("SQ",TVM$Description_Fin),1,0)
TVM$SQLFLAG	                <-   	ifelse(grepl("SQL",TVM$Description_Fin),1,0)
TVM$SRXFLAG	                <-   	ifelse(grepl("SRX",TVM$Description_Fin),1,0)
TVM$STACKFLAG	                <-   	ifelse(grepl("STACK",TVM$Description_Fin),1,0)
TVM$STAIRFLAG	                <-   	ifelse(grepl("STAIR",TVM$Description_Fin),1,0)
TVM$STALLFLAG	                <-   	ifelse(grepl("STALL",TVM$Description_Fin),1,0)
TVM$STATIONFLAG	                <-   	ifelse(grepl("STATION",TVM$Description_Fin),1,0)
TVM$STATUSFLAG	                <-   	ifelse(grepl("STATUS",TVM$Description_Fin),1,0)
TVM$STEPFLAG	                <-   	ifelse(grepl("STEP",TVM$Description_Fin),1,0)
TVM$STILEFLAG	                <-   	ifelse(grepl("STILE",TVM$Description_Fin),1,0)
TVM$STOPFLAG	                <-   	ifelse(grepl("STOP",TVM$Description_Fin),1,0)
TVM$STOPPEDWORKFLAG	                <-   	ifelse(grepl("STOPPED WORK",TVM$Description_Fin),1,0)
TVM$STORAGEFLAG	                <-   	ifelse(grepl("STORAGE",TVM$Description_Fin),1,0)
TVM$STRIPSFLAG	                <-   	ifelse(grepl("STRIPS",TVM$Description_Fin),1,0)
TVM$STUCKFLAG	                <-   	ifelse(grepl("STUCK",TVM$Description_Fin),1,0)
TVM$SUBMITFLAG	                <-   	ifelse(grepl("SUBMIT",TVM$Description_Fin),1,0)
TVM$SUCKFLAG	                <-   	ifelse(grepl("SUCK",TVM$Description_Fin),1,0)
TVM$SUNRISEFLAG	                <-   	ifelse(grepl("SUNRISE",TVM$Description_Fin),1,0)
TVM$SUPERVISORFLAG	                <-   	ifelse(grepl("SUPERVISOR",TVM$Description_Fin),1,0)
TVM$SUPPLYFLAG	                <-   	ifelse(grepl("SUPPLY",TVM$Description_Fin),1,0)
TVM$SURGEFLAG	                <-   	ifelse(grepl("SURGE",TVM$Description_Fin),1,0)
TVM$SURVEYFLAG	                <-   	ifelse(grepl("SURVEY",TVM$Description_Fin),1,0)
TVM$SVNFLAG	                <-   	ifelse(grepl("SVN",TVM$Description_Fin),1,0)
TVM$SWAPFLAG	                <-   	ifelse(grepl("SWAP",TVM$Description_Fin),1,0)
TVM$SWATCHFLAG	                <-   	ifelse(grepl("SWATCH",TVM$Description_Fin),1,0)
TVM$SWIFTPAGEFLAG	                <-   	ifelse(grepl("SWIFTPAGE",TVM$Description_Fin),1,0)
TVM$SWITCHFLAG	                <-   	ifelse(grepl("SWITCH",TVM$Description_Fin),1,0)
TVM$SYNCFLAG	                <-   	ifelse(grepl("SYNC",TVM$Description_Fin),1,0)
TVM$SYSTEMFLAG	                <-   	ifelse(grepl("SYSTEM",TVM$Description_Fin),1,0)
TVM$TABLEFLAG	                <-   	ifelse(grepl("TABLE",TVM$Description_Fin),1,0)
TVM$TABLETFLAG	                <-   	ifelse(grepl("TABLET",TVM$Description_Fin),1,0)
TVM$TAKECALLFLAG	                <-   	ifelse(grepl("TAKE CALL",TVM$Description_Fin),1,0)
TVM$TAKEFLAG	                <-   	ifelse(grepl("TAKE",TVM$Description_Fin),1,0)
TVM$TALYSTFLAG	                <-   	ifelse(grepl("TALYST",TVM$Description_Fin),1,0)
TVM$TAXFLAG	                <-   	ifelse(grepl("TAX",TVM$Description_Fin),1,0)
TVM$TEAMCENTERFLAG	                <-   	ifelse(grepl("TEAMCENTER",TVM$Description_Fin),1,0)
TVM$TEAMFLAG	                <-   	ifelse(grepl("TEAM",TVM$Description_Fin),1,0)
TVM$TEAMVIEWERFLAG	                <-   	ifelse(grepl("TEAM VIEWER",TVM$Description_Fin),1,0)
TVM$TEAMVIEWERFLAG2	                <-   	ifelse(grepl("TEAMVIEWER",TVM$Description_Fin),1,0)
TVM$TECHFLAG	                <-   	ifelse(grepl("TECH",TVM$Description_Fin),1,0)
TVM$TELNETFLAG	                <-   	ifelse(grepl("TELNET",TVM$Description_Fin),1,0)
TVM$TERMINATIONFLAG	                <-   	ifelse(grepl("TERMINATION",TVM$Description_Fin),1,0)
TVM$TESTFLAG	                <-   	ifelse(grepl("TEST",TVM$Description_Fin),1,0)
TVM$THEYFLAG	                <-   	ifelse(grepl("THEY",TVM$Description_Fin),1,0)
TVM$THINKFLAG	                <-   	ifelse(grepl("THINK",TVM$Description_Fin),1,0)
TVM$THRESHOLDFLAG	                <-   	ifelse(grepl("THRESHOLD",TVM$Description_Fin),1,0)
TVM$THRUFLAG	                <-   	ifelse(grepl("THRU",TVM$Description_Fin),1,0)
TVM$TICKETFLAG	                <-   	ifelse(grepl("TICKET",TVM$Description_Fin),1,0)
TVM$TIGERSTOPFLAG	                <-   	ifelse(grepl("TIGER STOP",TVM$Description_Fin),1,0)
TVM$TIMEFLAG	                <-   	ifelse(grepl("TIME",TVM$Description_Fin),1,0)
TVM$TOMCATFLAG	                <-   	ifelse(grepl("TOMCAT",TVM$Description_Fin),1,0)
TVM$TONERFLAG	                <-   	ifelse(grepl("TONER",TVM$Description_Fin),1,0)
TVM$TOOLFLAG	                <-   	ifelse(grepl("TOOL",TVM$Description_Fin),1,0)
TVM$TOUCHSCREENFLAG	                <-   	ifelse(grepl("TOUCHSCREEN",TVM$Description_Fin),1,0)
TVM$TOUCHSCREENFLAG2	                <-   	ifelse(grepl("TOUCH SCREEN",TVM$Description_Fin),1,0)
TVM$TOWERFLAG	                <-   	ifelse(grepl("TOWER",TVM$Description_Fin),1,0)
TVM$TRACKFLAG	                <-   	ifelse(grepl("TRACK",TVM$Description_Fin),1,0)
TVM$TRAFFICFLAG	                <-   	ifelse(grepl("TRAFFIC",TVM$Description_Fin),1,0)
TVM$TRAININGFLAG	                <-   	ifelse(grepl("TRAINING",TVM$Description_Fin),1,0)
TVM$TRANSACTIONFLAG	                <-   	ifelse(grepl("TRANSACTION",TVM$Description_Fin),1,0)
TVM$TRANSFERFLAG	                <-   	ifelse(grepl("TRANSFER",TVM$Description_Fin),1,0)
TVM$TRANSLATORFLAG	                <-   	ifelse(grepl("TRANSLATOR",TVM$Description_Fin),1,0)
TVM$TRANSULATORFLAG	                <-   	ifelse(grepl("TRANSULATOR",TVM$Description_Fin),1,0)
TVM$TRASULATORFLAG	                <-   	ifelse(grepl("TRASULATOR",TVM$Description_Fin),1,0)
TVM$TREEFLAG	                <-   	ifelse(grepl("TREE",TVM$Description_Fin),1,0)
TVM$TRIAGEFLAG	                <-   	ifelse(grepl("TRIAGE",TVM$Description_Fin),1,0)
TVM$TROUBLEFLAG	                <-   	ifelse(grepl("TROUBLE",TVM$Description_Fin),1,0)
TVM$TS3FLAG	                <-   	ifelse(grepl("TS3",TVM$Description_Fin),1,0)
TVM$TUNNELFLAG	                <-   	ifelse(grepl("TUNNEL",TVM$Description_Fin),1,0)
TVM$TURNINGONFLAG	                <-   	ifelse(grepl("TURNING ON",TVM$Description_Fin),1,0)
TVM$TURNONFLAG	                <-   	ifelse(grepl("TURN ON",TVM$Description_Fin),1,0)
TVM$TXMAPD01FLAG	                <-   	ifelse(grepl("TXMAPD01",TVM$Description_Fin),1,0)
TVM$UBABLEFLAG	                <-   	ifelse(grepl("UBABLE",TVM$Description_Fin),1,0)
TVM$UCCXFLAG	                <-   	ifelse(grepl("UCCX",TVM$Description_Fin),1,0)
TVM$UNABLEFLAG	                <-   	ifelse(grepl("UNABLE",TVM$Description_Fin),1,0)
TVM$UNABLLEFLAG	                <-   	ifelse(grepl("UNABLLE",TVM$Description_Fin),1,0)
TVM$UNAVAILABLEFLAG	                <-   	ifelse(grepl("UNAVAILABLE",TVM$Description_Fin),1,0)
TVM$UNAVAILABILITYFLAG	                <-   	ifelse(grepl("UNAVAILABILITY",TVM$Description_Fin),1,0)
TVM$UNITFLAG	                <-   	ifelse(grepl("UNIT",TVM$Description_Fin),1,0)
TVM$UNKNOWNFLAG	                <-   	ifelse(grepl("UNKNOWN",TVM$Description_Fin),1,0)
TVM$UNPLUGGEDFLAG	                <-   	ifelse(grepl("UNPLUGGED",TVM$Description_Fin),1,0)
TVM$UPDATFLAG	                <-   	ifelse(grepl("UPDAT",TVM$Description_Fin),1,0)
TVM$UPDATEFLAG	                <-   	ifelse(grepl("UPDATE",TVM$Description_Fin),1,0)
TVM$UPFLAG	                <-   	ifelse(grepl("UP",TVM$Description_Fin),1,0)
TVM$UPGRADFLAG	                <-   	ifelse(grepl("UPGRAD",TVM$Description_Fin),1,0)
TVM$UPGRADEFLAG	                <-   	ifelse(grepl("UPGRADE",TVM$Description_Fin),1,0)
TVM$UPLINKFLAG	                <-   	ifelse(grepl("UPLINK",TVM$Description_Fin),1,0)
TVM$UPLOADFLAG	                <-   	ifelse(grepl("UPLOAD",TVM$Description_Fin),1,0)
TVM$UPSFLAG	                <-   	ifelse(grepl("UPS",TVM$Description_Fin),1,0)
TVM$UPSIDEDOWNFLAG	                <-   	ifelse(grepl("UPSIDE DOWN",TVM$Description_Fin),1,0)
TVM$URLFLAG	                <-   	ifelse(grepl("URL",TVM$Description_Fin),1,0)
TVM$USAGEFLAG	                <-   	ifelse(grepl("USAGE",TVM$Description_Fin),1,0)
TVM$USBFLAG	                <-   	ifelse(grepl("USB",TVM$Description_Fin),1,0)
TVM$USERFLAG	                <-   	ifelse(grepl("USER",TVM$Description_Fin),1,0)
TVM$USERSFLAG	                <-   	ifelse(grepl("USERS",TVM$Description_Fin),1,0)
TVM$VALANCEFLAG	                <-   	ifelse(grepl("VALANCE",TVM$Description_Fin),1,0)
TVM$VENDORFLAG	                <-   	ifelse(grepl("VENDOR",TVM$Description_Fin),1,0)
TVM$VERIFYFLAG	                <-   	ifelse(grepl("VERIFY",TVM$Description_Fin),1,0)
TVM$VIEWFLAG	                <-   	ifelse(grepl("VIEW",TVM$Description_Fin),1,0)
TVM$VIGNETTEFLAG	                <-   	ifelse(grepl("VIGNETTE",TVM$Description_Fin),1,0)
TVM$VIRTUALMACHINEFLAG	                <-   	ifelse(grepl("VIRTUAL MACHINE",TVM$Description_Fin),1,0)
TVM$VIRUSFLAG	                <-   	ifelse(grepl("VIRUS",TVM$Description_Fin),1,0)
TVM$VISIBLEFLAG	                <-   	ifelse(grepl("VISIBLE",TVM$Description_Fin),1,0)
TVM$VISIOFLAG	                <-   	ifelse(grepl("VISIO",TVM$Description_Fin),1,0)
TVM$VLANFLAG	                <-   	ifelse(grepl("VLAN",TVM$Description_Fin),1,0)
TVM$VMFLAG	                <-   	ifelse(grepl("VM",TVM$Description_Fin),1,0)
TVM$VMWAREFLAG	                <-   	ifelse(grepl("VMWARE",TVM$Description_Fin),1,0)
TVM$VOICEMAILFLAG	                <-   	ifelse(grepl("VOICEMAIL",TVM$Description_Fin),1,0)
TVM$VOICEMAILFLAG2	                <-   	ifelse(grepl("VOICE MAIL",TVM$Description_Fin),1,0)
TVM$VOIPFLAG	                <-   	ifelse(grepl("VOIP",TVM$Description_Fin),1,0)
TVM$VOLUMEFLAG	                <-   	ifelse(grepl("VOLUME",TVM$Description_Fin),1,0)
TVM$VPNFLAG	                <-   	ifelse(grepl("VPN",TVM$Description_Fin),1,0)
TVM$WARNINGFLAG	                <-   	ifelse(grepl("WARNING",TVM$Description_Fin),1,0)
TVM$WARRANTYFLAG	                <-   	ifelse(grepl("WARRANTY",TVM$Description_Fin),1,0)
TVM$WASPFLAG	                <-   	ifelse(grepl("WASP",TVM$Description_Fin),1,0)
TVM$WATERVLIETFLAG	                <-   	ifelse(grepl("WATERVLIET",TVM$Description_Fin),1,0)
TVM$WEBFLAG	                <-   	ifelse(grepl("WEB",TVM$Description_Fin),1,0)
TVM$WEBEXFLAG	                <-   	ifelse(grepl("WEBEX",TVM$Description_Fin),1,0)
TVM$WEBEXFLAG2	                <-   	ifelse(grepl("WEB EX",TVM$Description_Fin),1,0)
TVM$WEBSITEFLAG	                <-   	ifelse(grepl("WEBSITE",TVM$Description_Fin),1,0)
TVM$WECANTLOGINFLAG	                <-   	ifelse(grepl("WE CANT LOG IN",TVM$Description_Fin),1,0)
TVM$WEFLAG	                <-   	ifelse(grepl("WE ",TVM$Description_Fin),1,0)
TVM$WGFLAG	                <-   	ifelse(grepl("WG",TVM$Description_Fin),1,0)
TVM$WHENFLAG	                <-   	ifelse(grepl("WHEN",TVM$Description_Fin),1,0)
TVM$WIFIFLAG	                <-   	ifelse(grepl("WIFI",TVM$Description_Fin),1,0)
TVM$WIKIFLAG	                <-   	ifelse(grepl("WIKI",TVM$Description_Fin),1,0)
TVM$WILEYFLAG	                <-   	ifelse(grepl("WILEY",TVM$Description_Fin),1,0)
TVM$WILLNOTFLAG	                <-   	ifelse(grepl("WILL NOT",TVM$Description_Fin),1,0)
TVM$WILLNOTTAKEFLAG	                <-   	ifelse(grepl("WILL NOT TAKE",TVM$Description_Fin),1,0)
TVM$WINFLAG	                <-   	ifelse(grepl("WIN",TVM$Description_Fin),1,0)
TVM$WINDOWSFLAG	                <-   	ifelse(grepl("WINDOWS",TVM$Description_Fin),1,0)
TVM$WINZIPFLAG	                <-   	ifelse(grepl("WINZIP",TVM$Description_Fin),1,0)
TVM$WIRELESSFLAG	                <-   	ifelse(grepl("WIRELESS",TVM$Description_Fin),1,0)
TVM$WITHOUTFLAG	                <-   	ifelse(grepl("WITHOUT",TVM$Description_Fin),1,0)
TVM$WONTFEEDFLAG	                <-   	ifelse(grepl("WONT FEED",TVM$Description_Fin),1,0)
TVM$WONTFLAG	                <-   	ifelse(grepl("WONT",TVM$Description_Fin),1,0)
TVM$WONTHOLDFLAG	                <-   	ifelse(grepl("WONT HOLD",TVM$Description_Fin),1,0)
TVM$WONTWORKFLAG	                <-   	ifelse(grepl("WONT WORK",TVM$Description_Fin),1,0)
TVM$WOODFLAG	                <-   	ifelse(grepl("WOOD",TVM$Description_Fin),1,0)
TVM$WORKFLAG	                <-   	ifelse(grepl("WORK",TVM$Description_Fin),1,0)
TVM$WORKINGFLAG	                <-   	ifelse(grepl("WORKING",TVM$Description_Fin),1,0)
TVM$WRAPFLAG	                <-   	ifelse(grepl("WRAP",TVM$Description_Fin),1,0)
TVM$WRITEFLAG	                <-   	ifelse(grepl("WRITE",TVM$Description_Fin),1,0)
TVM$WRONGFLAG	                <-   	ifelse(grepl("WRONG",TVM$Description_Fin),1,0)
TVM$WYSETERMINALSFLAG	                <-   	ifelse(grepl("WYSE TERMINALS",TVM$Description_Fin),1,0)
TVM$XMLFLAG	                <-   	ifelse(grepl("XML",TVM$Description_Fin),1,0)
TVM$ZEBRAFLAG	                <-   	ifelse(grepl("ZEBRA",TVM$Description_Fin),1,0)
TVM$ZEEBRAFLAG	                <-   	ifelse(grepl("ZEEBRA",TVM$Description_Fin),1,0)
TVM$ZENWORKSFLAG	                <-   	ifelse(grepl("ZENWORKS",TVM$Description_Fin),1,0)
TVM$ZIPFLAG	                <-   	ifelse(grepl("ZIP",TVM$Description_Fin),1,0)

TVM$EMAILFLAG   <- ifelse (TVM$EMAILFLAG > 0 | TVM$EMAILFLAG2 > 0 | TVM$EMAILFLAG3 > 0 | TVM$EMAILFLAG4 > 0,1,0)
TVM$FREEZEFLAG  <- ifelse (TVM$FREEZEDFLAG > 0 | TVM$FREEZESFLAG > 0 | TVM$FREEZESSFLAG > 0 | TVM$FREEZINGFLAG > 0 |TVM$FROZEFLAG > 0 | TVM$FROZENFLAG > 0 | TVM$FREEZFLAG > 0,1,0)
TVM$UNABLEFLAG  <- ifelse (TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | TVM$NOTWORKFLAG > 0,1,0)
TVM$LABLEFLAG   <- ifelse (TVM$LABLEFLAG > 0 & TVM$AVAILABLEFLAG > 0,0,paste(TVM$LABLEFLAG))
TVM$LABELFLAG   <- ifelse (TVM$LABELFLAG > 0 | TVM$LABLEFLAG > 0,1,0)
TVM$LOGOUTFLAG  <- ifelse (TVM$LOGSOFFFLAG > 0 | TVM$LOGOUTFLAG > 0,1,0)
TVM$LOGINFLAG   <- ifelse (TVM$LOGINFLAG > 0 | TVM$LOGINFLAG2 > 0 | TVM$LOGONFLAG > 0 | TVM$LOGONFLAG2 > 0 | TVM$LOGGINGFLAG > 0,1,0)
TVM$PERFORMANCEFLAG <- ifelse (TVM$PERFORMANCEFLAG > 0 | TVM$PERFORMINGFLAG > 0,1,0)
TVM$TRANSLATORFLAG  <- ifelse (TVM$TRANSLATORFLAG > 0 | TVM$TRANSULATORFLAG > 0 | TVM$TRASULATORFLAG > 0,1,0)
TVM$RESTARTEDFLAG   <- ifelse (TVM$RESTARTEDFLAG > 0 | TVM$RESTARTINGFLAG > 0,1,0)
TVM$BACKINGFLAG     <- ifelse (TVM$BACKINGFLAG > 0 | TVM$BACKINFLAG > 0 | TVM$BACKINGUPFLAG > 0,1,0)
TVM$DOWNFLAG    <- ifelse (TVM$DOWNFLAG > 0 & (TVM$DOWNLOADFLAG > 0 | TVM$DOWNSTAIRFLAG > 0 | TVM$DROPFLAG > 0 | TVM$STAIRFLAG > 0),0,paste(TVM$DOWNFLAG))
TVM$NOTFLAG     <- ifelse (TVM$NOTFLAG > 0 & (TVM$NOTESFLAG > 0 | TVM$NOTICEFLAG > 0),0,paste(TVM$NOTFLAG))
TVM$NOFLAG      <- ifelse (TVM$NOFLAG > 0 & (TVM$NOTESFLAG > 0 | TVM$KNOWFLAG > 0 | TVM$NONFLAG > 0),0,paste(TVM$NOFLAG))
TVM$NEWFLAG     <- ifelse (TVM$NEWFLAG > 0 & TVM$RENEWFLAG > 0,0,paste(TVM$NEWFLAG))
TVM$STALLFLAG   <- ifelse (TVM$STALLFLAG > 0 & TVM$INSTALLFLAG > 0,0,paste(TVM$STALLFLAG))
TVM$FRAMEFLAG   <- ifelse (TVM$FRAMEFLAG > 0 & TVM$PLATEFLAG > 0,0,paste(TVM$FRAMEFLAG))
TVM$USERSFLAG   <- ifelse (TVM$USERSFLAG > 0 | TVM$NONEOFTHEUSERFLAG > 0 | TVM$NONOFTHEUSERFLAG > 0 | TVM$MANYUSERFLAG > 0 | TVM$ENTIREFLOORFLAG > 0 | TVM$COMPUTERSFLAG > 0 | TVM$ENTIRETEAMFLAG > 0,1,0)
TVM$RESETFLAG   <- ifelse (TVM$RESETFLAG > 0 | TVM$RESETFLAG2 > 0,1,0)
TVM$PASSWORDFLAG   <- ifelse (TVM$PASSWORDFLAG > 0 | TVM$PWFLAG > 0,1,0) 
TVM$VOICEMAILFLAG  <- ifelse (TVM$VOICEMAILFLAG > 0 | TVM$VOICEMAILFLAG2 > 0,1,0)
TVM$HEADSETFLAG    <- ifelse (TVM$HEADSETFLAG > 0 | TVM$HEADSETFLAG2 > 0,1,0)
TVM$CANNOTFLAG     <- ifelse (TVM$CANNOTFLAG > 0 | TVM$CANNOTFLAG2 > 0,1,0)
TVM$BARCODEFLAG    <- ifelse (TVM$BARCODEFLAG > 0 | TVM$BARCODEFLAG2 > 0,1,0)
TVM$TOUCHSCREENFLAG <- ifelse (TVM$TOUCHSCREENFLAG > 0 | TVM$TOUCHSCREENFLAG2 > 0,1,0)
TVM$INKFLAG       <- ifelse (TVM$INKFLAG > 0 & (TVM$THINKFLAG > 0 | TVM$LINKFLAG > 0),0,paste(TVM$INKFLAG))
TVM$INTERNFLAG    <- ifelse (TVM$INTERNFLAG > 0 & (TVM$INTERNETFLAG > 0 | TVM$INTERNALFLAG > 0),0,paste(TVM$INTERNFLAG))
TVM$ROLLERFLAG    <- ifelse (TVM$ROLLERFLAG > 0 & TVM$CONTROLLERFLAG > 0,0,paste(TVM$ROLLERFLAG))
TVM$DIRECTWEBFLAG <- ifelse (TVM$DIRECTWEBFLAG > 0 | TVM$DIRECTWEBFLAG2 > 0,1,0)
TVM$ININFLAG      <- ifelse (TVM$ININFLAG > 0 & TVM$TRAININGFLAG > 0,0,paste(TVM$ININFLAG))
TVM$LINKFLAG      <- ifelse (TVM$LINKFLAG > 0 & TVM$LINKAGFLAG > 0,0,paste(TVM$LINKFLAG))
TVM$AREFLAG       <- ifelse (TVM$AREFLAG > 0 & TVM$AREAFLAG > 0,0,paste(TVM$AREFLAG))
TVM$ALLFLAG       <- ifelse (TVM$ALLFLAG > 0 & (TVM$ALLANFLAG > 0 | TVM$ALLOWFLAG > 0),0,paste(TVM$ALLFLAG))
TVM$SAPFLAG       <- ifelse (TVM$SAPFLAG > 0 & TVM$ASAPFLAG > 0,0,paste(TVM$SAPFLAG))
TVM$SETUPFLAG     <- ifelse (TVM$SETUPFLAG > 0 | TVM$SETUPFLAG2 > 0,1,0)
TVM$OFFLINEFLAG   <- ifelse (TVM$OFFLINEFLAG > 0 | TVM$OFFLINEFLAG2 > 0,1,0)
TVM$WEBEXFLAG     <- ifelse (TVM$WEBEXFLAG > 0 | TVM$WEBEXFLAG2 > 0,1,0)
TVM$HELPDESKFLAG  <- ifelse (TVM$HELPDESKFLAG > 0 | TVM$HELPDESKFLAG2 > 0,1,0)
TVM$ITSUPPORTFLAG <- ifelse (TVM$ITSUPPORTFLAG > 0 | TVM$ITSUPPORTFLAG2 > 0,1,0)


TVM$KEYWORDVAR <- ifelse (TVM$LABELFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
                          TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 |
                          TVM$UBABLEFLAG > 0 | TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 |
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 |
                          TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 |
                          TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 |
                          TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | 
                          TVM$NEWFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 |
                          TVM$OFFCENTERFLAG > 0 | 
                          #TVM$OUTFLAG > 0 | 
                          TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | TVM$STALLFLAG > 0 |
                          TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 |
                          TVM$WONTFEEDFLAG > 0) & TVM$PRINTFLAG > 0,"1 PRINT LABEL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ZEBRAFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
                          TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 |
                          TVM$UBABLEFLAG > 0 | TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 |
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 |
			                    TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 |
			                    TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | 
                          TVM$LONGFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
                          TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
                          #TVM$OUTFLAG > 0 | 
                          TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | TVM$STALLFLAG > 0 |
			                    TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 |
			                    TVM$WONTFEEDFLAG > 0) & TVM$PRINTFLAG > 0,"2 PRINT ZEBRA ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WASPFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CANNOTFLAG > 0 |
			                    TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 |
			                    TVM$DOESNTWORKFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 |
			                    TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 |
 			                    #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
			                    TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 |
			                    #TVM$OUTFLAG > 0 | 
			                    TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | TVM$STALLFLAG > 0 |
			                    TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 |
			                    TVM$WONTFEEDFLAG > 0) & TVM$PRINTFLAG > 0,"3 PRINT WASP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WRAPFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CANNOTFLAG > 0 |
			                    TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 |
			                    TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 |
			                    TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
	 		                    #TVM$NEEDFLAG > 0 |TVM$NEWFLAG > 0 |
			                    TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | 
                          TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
			                    #TVM$OUTFLAG > 0 | 
		  	                  TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                          TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"4 PRINT WRAP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WOODFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                          TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                          TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 |
 			                    #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
			                    TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 |
			                    #TVM$OUTFLAG > 0 |
			                    TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | TVM$STALLFLAG > 0 |
			                    TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 |
			                    TVM$WONTFEEDFLAG > 0) & TVM$PRINTFLAG > 0,"5 PRINT WOOD ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STATIONFLAG > 0 & 
                         (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                          TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                          TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 |
			                    #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
			                    TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 |
			                    #TVM$OUTFLAG > 0 | 
			                    TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                          TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"6 PRINT STATION ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$KAIZENFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
                          TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | 
                          #TVM$HALFFLAG > 0 |
                          TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          #TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | 
                          TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 |
                          TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
                          #TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 |
                          TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
                          TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
                          #TVM$OUTFLAG > 0 | 
                          TVM$POORFLAG > 0 | 
                          TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | 
                          #REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 |
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 |
                          #TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 | 
                          TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"7 PRINT KAIZEN ISSUE",paste(TVM$KEYWORDVAR))
                          
TVM$KEYWORDVAR <- ifelse (TVM$FRAMEFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
			                    TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 |
			                    TVM$UBABLEFLAG > 0 | TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | 
			                    TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 |
			                    TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
                          TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 |
                          #TVM$OUTFLAG > 0 | 
                          TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                          TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"8 PRINT FRAME ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ALTERFLAG > 0 & 
                         (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                          TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                          TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
			                    #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
			                    TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
			                    #TVM$OUTFLAG > 0 |
			                    TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                          TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"9 PRINT ALTER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$ATSFLAG > 0 & TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
                          TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | 
                          #TVM$HALFFLAG > 0 |
                          TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          #TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 |
                          TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                          TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
                          #TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 |
                          TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
                          TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
                          #TVM$OUTFLAG > 0 | 
                          TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | 
                          #TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 |
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | 
                          #TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | TVM$UPSIDEDOWNFLAG > 0 |
                          TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0) & TVM$PRINTFLAG > 0 & TVM$SHIPFLAG > 0,"10 PRINT ATS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$BOXINGFLAG > 0 & 
                         (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                          TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                          TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                          TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                          TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
                          #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
                          TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
                          #TVM$OUTFLAG > 0 |
                          TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                          TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                          TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"11 PRINT BOXING ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAPFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                          TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                          TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$FIXFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | 
                          TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$STALLFLAG > 0)
                          & TVM$PRINTFLAG > 0 & TVM$LAPTOPFLAG == 0 & TVM$WEBSITEFLAG == 0,"12 PRINT SAP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAWFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                               TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                               TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
			       #TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
			       TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
			       #TVM$OUTFLAG > 0 |
			       TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                               TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                          & TVM$PRINTFLAG > 0,"13 PRINT SAW ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STILEFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 |
			  TVM$NOTABLEFLAG > 0 | TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 |
 TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | 
                              TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"14 PRINT STILE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$BARCODEFLAG > 0 & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"15 PRINT BARCODE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$PACKLISTFLAG > 0 | TVM$PACKINGLISTFLAG > 0 | TVM$PACKINGSLIPFLAG > 0) & (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 |
 TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | 
                              TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"16 PRINT PACKLIST ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TONERFLAG > 0 & (TVM$CHANGEFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0
                       | TVM$OUTFLAG > 0 | TVM$POORFLAG > 0 | TVM$REPLACEFLAG > 0)
                     & TVM$PRINTFLAG > 0,"17 PRINT TONER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INKFLAG > 0 & (TVM$CHANGEFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0| 
			  TVM$OUTFLAG > 0 | TVM$POORFLAG > 0 | TVM$REPLACEFLAG > 0) & TVM$PRINTFLAG > 0
                          ,"18 PRINT INK ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$FUSERFLAG > 0 & (TVM$CHANGEFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
			  TVM$OUTFLAG > 0 | TVM$POORFLAG > 0 | TVM$REPLACEFLAG > 0)
                          # & TVM$PRINTFLAG > 0 
                          ,"19 PRINT FUSER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RIBBONFLAG > 0 & (TVM$CHANGEFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$OUTFLAG > 0 |
                          TVM$POORFLAG > 0 | TVM$REPLACEFLAG > 0) & TVM$PRINTFLAG > 0,"20 PRINT RIBBON ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ROLLERFLAG > 0 & (TVM$CHANGEFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0
                       | TVM$OUTFLAG > 0 | TVM$POORFLAG > 0 | TVM$REPLACEFLAG > 0)
                     & TVM$PRINTFLAG > 0
                          ,"21 PRINT ROLLER ISSUE",paste(TVM$KEYWORDVAR))


TVM$KEYWORDVAR <- ifelse (TVM$PRODUCTIONFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | TVM$CHANGEFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                              TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$SNAPFLAG > 0 | 
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0 & TVM$AREAFLAG > 0,"22 PRINT PRODUCTON ISSUE",paste(TVM$KEYWORDVAR))
                          
TVM$KEYWORDVAR <- ifelse ((TVM$COPIERFLAG > 0 | TVM$COPYMACHINEFLAG > 0) & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 |
TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 | 
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 | 
                               TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0
                          ,"23 PRINT COPIER ISSUE",paste(TVM$KEYWORDVAR))


TVM$KEYWORDVAR <- ifelse (TVM$HARDWAREFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 |
TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 |
                              TVM$STALLFLAG > 0 | TVM$STOPFLAG > 0 | TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 | 
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"24 PRINT EHARDWARE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ORDERFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 |
TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | 
#TVM$LINESFLAG > 0 | 
TVM$LONGFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 | 
TVM$STALLFLAG > 0 | 
#TVM$STOPFLAG > 0 | 
TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 | 
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"25 PRINT ORDER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SHIPFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 | 
TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | 
#TVM$LINESFLAG > 0 | 
TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 | 
TVM$STALLFLAG > 0 | 
#TVM$STOPFLAG > 0 |
TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 |
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"26 PRINT SHIP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MARKETFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 |
TVM$DOESNOTFLAG > 0 | 
                               TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 | 
TVM$STALLFLAG > 0 | 
#TVM$STOPFLAG > 0 | 
TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 | 
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"27 PRINT MRKT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PARTSFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 | 
TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 | 
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 | 
TVM$STALLFLAG > 0 | 
#TVM$STOPFLAG > 0 | 
TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 | 
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0,"28 PRINT PARTS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PACKFLAG > 0 & 
                            (TVM$UNABLEFLAG > 0 | TVM$DELAYFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$NOTFLAG > 0 | TVM$NOFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
                               TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$HALFFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
                               TVM$SIDEWAYSFLAG > 0 | TVM$BLANKFLAG > 0 | TVM$BLURFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
                               TVM$CANNOTFLAG > 0 | TVM$CANNOTGETFLAG > 0 | 
#TVM$CHANGEFLAG > 0 | 
TVM$DOESNOTFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNTFEEDFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
                              TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$EXCELFLAG > 0 | TVM$FEEDFLAG > 0 | TVM$FIXFLAG > 0 | 
                               TVM$FROZFLAG > 0 | TVM$ISNTFLAG > 0 | TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LONGFLAG > 0 | 
                              TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | TVM$MISPRINTFLAG > 0 | 
#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | 
#TVM$OUTFLAG > 0 |
TVM$POORFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
#TVM$SNAPFLAG > 0 |
TVM$STALLFLAG > 0 | 
#TVM$STOPFLAG > 0 |
TVM$STUCKFLAG > 0 | 
#TVM$SUCKFLAG > 0 |
TVM$UPSIDEDOWNFLAG > 0 | TVM$WILLNOTTAKEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$WONTFEEDFLAG > 0)
                     & TVM$PRINTFLAG > 0 ,"29 PRINT PACK ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$FAXFLAG > 0 & TVM$NOTRECEIVFLAG > 0) | (TVM$FAXFLAG > 0 & 
		(TVM$NOTRECEIVFLAG > 0 | TVM$DELAYFLAG > 0 | 
		TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
		TVM$DOWNFLAG > 0 | TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | 
		TVM$JAMFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | 
		TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | TVM$POORFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
		TVM$STALLFLAG > 0 | TVM$STUCKFLAG > 0) & TVM$MACHINEFLAG > 0) | (TVM$FAXFLAG > 0 & 
		(TVM$ISSUEFLAG > 0 | TVM$NOTRECEIVFLAG > 0 | TVM$DELAYFLAG > 0 | 
		TVM$SLOWFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | 
		TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$FIXFLAG > 0 | TVM$FROZFLAG > 0 | 
		TVM$JAMFLAG > 0 | TVM$LINESFLAG > 0 | TVM$LOWQUALITYFLAG > 0 | TVM$MISALIGNEDFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTGETFLAG > 0 | 
		TVM$NOTRESPONDFLAG > 0 | TVM$OFFCENTERFLAG > 0 | TVM$POORFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | 
		TVM$STALLFLAG > 0 | TVM$STUCKFLAG > 0) & (TVM$SEVERALFLAG > 0 | TVM$NOTBEENCOMINGFLAG > 0))
		,"30 FAX MACHINE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$STATIONFLAG > 0,"31 SCAN STATION ISSUE",paste(TVM$KEYWORDVAR))
TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$TABLEFLAG > 0,"32 SCAN TABLE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$KIOSKFLAG > 0,"33 SCAN KIOSK ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$CARTFLAG > 0,"34 SCAN CART ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$HOISTFLAG > 0,"35 SCAN HOIST ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$ASSEMBLYFLAG > 0,"36 SCAN ASSEMBLY ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$KITACEFLAG > 0,"37 SCAN KITACE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$BARCODEFLAG > 0,"38 SCAN BARCODE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$PARTSFLAG > 0,"39 SCAN PARTS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$FABRICFLAG > 0,"40 SCAN FABRIC ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & (TVM$HARDWAREFLAG > 0
    # | TVM$SCREENFLAG > 0 | TVM$SAWFLAG > 0 | TVM$PACKFLAG > 0 | TVM$SORTFLAG > 0 | TVM$SYSTEMFLAG > 0 | TVM$FINISHFLAG > 0
		),"41 SCAN EHDWARE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$SAWFLAG > 0,"42 SCAN SAW ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		#TVM$NEEDFLAG > 0 |
TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$PACKFLAG > 0,"43 SCAN PACK ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$SORTFLAG > 0,"44 SCAN SORT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
		TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$SYSTEMFLAG > 0,"45 SCAN SYSTEM ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$SCREENFLAG > 0,"46 SCAN SCREEN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0)
		 & TVM$FAUXFLAG > 0,"47 SCAN FAUX ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0 | TVM$NOTALLOWFLAG > 0)
		 & TVM$KAIZENFLAG > 0,"48 SCAN KAIZEN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$CHARGFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 |
		TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0 | TVM$NOTALLOWFLAG > 0)
		 & TVM$COMPUTERFLAG > 0,"49 SCAN COMPUTER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTCHARGFLAG > 0 | TVM$BROKEFLAG > 0 | TVM$CALIBRATFLAG > 0 | TVM$DEADFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		#TVM$NEEDFLAG > 0 | TVM$NEWFLAG > 0 | 
		TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0 | TVM$NOTALLOWFLAG > 0)
		 & (TVM$CHARGFLAG > 0 | TVM$POWERFLAG > 0 | TVM$CONNECTFLAG > 0 | TVM$BATTERYFLAG > 0 | TVM$BASEFLAG > 0 | TVM$TRANSFERFLAG > 0
		#  | TVM$DOCFLAG > 0 | TVM$LINKFLAG> 0
		),"50 SCAN CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0 | 
		TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$ERRORFLAG > 0 | 
		TVM$DROPFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$NOTPICKUPFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$WONTHOLDFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$NOTKEEPFLAG > 0 | 
		TVM$CANNOTGETFLAG > 0 | TVM$NOTGETFLAG > 0 | TVM$CANTGETFLAG > 0 | TVM$NOINFOFLAG > 0 | 
		TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$REMOVEFLAG > 0 | TVM$NOTALLOWFLAG > 0)
		 & TVM$ORDERFLAG > 0
		,"51 SCAN ORDER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & (TVM$NEEDFLAG > 0 | TVM$REPLACEFLAG > 0) & TVM$LOOSFLAG > 0 & TVM$PRODUCTIONFLAG > 0 & TVM$TIMEFLAG > 0
		,"52 SCAN PRODTIME ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANNERFLAG > 0 & TVM$NOTSCANFLAG > 0,"53 SCAN NOSCAN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$BARCODEFLAG > 0 & TVM$NOTSCANFLAG > 0,"54 SCAN BARCODE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0)
		 & TVM$SERVICEFLAG > 0 & (TVM$ERRORFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"55 DWEB SERVICE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$WILLNOTFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
		TVM$CANNOTFLAG > 0) & TVM$PULLUPFLAG > 0 & TVM$SITEFLAG > 0 & TVM$PASSWORDFLAG == 0,"56 DWEB ACCESS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$WILLNOTFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | 
		TVM$CANNOTFLAG > 0) & TVM$GETINFLAG > 0 & TVM$PASSWORDFLAG == 0,"57 DWEB ACCESS ISSUE2",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$SLOWFLAG > 0) & (TVM$ERRORFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"58 DWEB SLOW ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 |
		  TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0) & (TVM$CSRSFLAG > 0 | TVM$USERSFLAG > 0)
		,"59 DWEB USERS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & 
		(TVM$DOESNOTFLAG > 0 | TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$CANNOTFLAG > 0 | TVM$WILLNOTFLAG > 0 | 
		TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0)
		 & TVM$WORKFLAG > 0 & TVM$PASSWORDFLAG == 0,"60 DWEB NOT WORK ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$RESETFLAG > 0 | TVM$RESTARTFLAG > 0) & TVM$SERVERFLAG > 0
		,"61 DWEB SERVER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 |
		 TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0) & TVM$WEBSITEFLAG > 0 & TVM$CHARTFLAG == 0
		,"62 DWEB SITE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DIRECTWEBFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
		 TVM$NOTACCESSFLAG > 0) & TVM$CHARTFLAG == 0,"63 DWEB DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CLOCKFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
			 TVM$NOTACCESSFLAG > 0 | TVM$REPLACEFLAG > 0) 
# & BROOMFIELDFLAG > 0
		,"64 CLOCK DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CLOCKFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$NOTEXCEPTFLAG > 0 | TVM$NOTGOINGFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$SLOWFLAG > 0 | 
		TVM$NOTUPDATFLAG > 0) & 
		(TVM$PUNCHFLAG > 0 | TVM$TIMEFLAG > 0 | TVM$KRONOSFLAG > 0 | TVM$SCANFLAG > 0 | TVM$RECORDFLAG > 0 | TVM$ACCESSFLAG > 0 | TVM$LOGINFLAG > 0)
		,"65 CLOCK PROB ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$KRONOSFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$NOTEXCEPTFLAG > 0 | TVM$NOTGOINGFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$SLOWFLAG > 0 | 
		TVM$NOTUPDATFLAG > 0) & 
		(TVM$PUNCHFLAG > 0 | TVM$TIMEFLAG > 0 | TVM$SCANFLAG > 0 | TVM$RECORDFLAG > 0 | TVM$ACCESSFLAG > 0 | TVM$LOGINFLAG > 0) & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0)
		,"66 KRONOS USERS ISSUE",paste(TVM$KEYWORDVAR))
TVM$KEYWORDVAR <- ifelse (TVM$KRONOSFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | 
		TVM$NOTACCEPTFLAG > 0 | TVM$NOTEXCEPTFLAG > 0 | TVM$NOTGOINGFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$SLOWFLAG > 0 | 
		TVM$NOTUPDATFLAG > 0) & 
		(
#TVM$PUNCHFLAG > 0 | TVM$TIMEFLAG > 0 | TVM$SCANFLAG > 0 | TVM$RECORDFLAG > 0 | TVM$ACCESSFLAG > 0 | TVM$LOGINFLAG > 0 |
TVM$SERVERFLAG > 0) & TVM$JAVAFLAG == 0 & TVM$VPNFLAG == 0 & TVM$REMOTEFLAG == 0,"67 KRONOS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERFACEFLAG > 0 & TVM$DOWNFLAG > 0 & TVM$NODEFLAG > 0,"68 INTERFACE DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERFACEFLAG > 0 & TVM$HIGHFLAG > 0 & TVM$TRAFFICFLAG > 0,"69 INTERFACE TRAFFIC",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERFACEFLAG > 0 & TVM$FLUCUATFLAG > 0,"70 INTERFACE FLUCUATED",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERFACEFLAG > 0 & TVM$OUTAGEFLAG > 0,"71 INTERFACE OUTAGE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NODEFLAG > 0 & TVM$DOWNFLAG > 0 & TVM$MONITORFLAG > 0,"72 NODE DOWN MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NODEFLAG > 0 & TVM$UPFLAG > 0 & TVM$PRIORITYFLAG > 0,"73 NODE UP MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NODEFLAG > 0 & TVM$UPFLAG > 0 & TVM$MONITORFLAG > 0,"74 NODE UP MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PACKETFLAG > 0 & TVM$LOSSFLAG > 0 & TVM$HIGHFLAG > 0,"75 HIGH PACKET LOSS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$XMLFLAG > 0 & TVM$MONITORFLAG > 0,"76 XML MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$FSFLAG > 0 & TVM$ALERTFLAG > 0 & TVM$THRESHOLDFLAG > 0,"77 FS ALERT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & TVM$DOWNFLAG > 0 & TVM$MONITORFLAG > 0,"78 NETWORK DOWNMONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & TVM$PERFORMANCEFLAG > 0 & TVM$MONITORFLAG > 0,"79 NETWORK MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NOTIFICATIONFLAG > 0 & TVM$HIGHFLAG > 0 & TVM$TRAFFICFLAG > 0,"80 TRAFFIC MONITOR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ADAPTERSFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$WIRELESSFLAG > 0
		,"81 ADAPTERS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ATSFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0) & 
		TVM$SYSTEMFLAG > 0 & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0),"82 ATS DOWNUSERS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ATSFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0)
		,"83 ATS USERS ERROR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ATSFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SERVERFLAG > 0 & TVM$DELAYFLAG > 0
		,"84 ATS SERVER ERROR",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ATSFLAG > 0 & TVM$MISSFLAG > 0 & TVM$BINSFLAG > 0 & TVM$STORAGEFLAG > 0,"85 ATS STORAGE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ATSFLAG > 0 & TVM$NOSHOWFLAG > 0 & TVM$ORDERFLAG > 0,"86 ATS ORDER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$BOXINGFLAG > 0 & TVM$NOTFLAG > 0 & TVM$CONNECTFLAG > 0 & TVM$STATIONFLAG > 0,"87 BOXINGCONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$BUILDINGFLAG > 0 & TVM$NOFLAG > 0 & TVM$POWERFLAG > 0,"88 BLDG NO POWER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CALLCENTERFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0) & TVM$TAKECALLFLAG > 0,"89 CALLCENTER NO CALL",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CALLFLAG > 0 & TVM$MISSFLAG > 0 & TVM$DATAFLAG > 0,"90 CALL MISS DATA",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CALLFLAG > 0 & TVM$NOTBACKFLAG > 0 & TVM$QUEUEFLAG > 0,"91 CALL QUEUE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERSFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0) & 
		(TVM$EVERYBODYFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0),"92 COMPUTERS DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERSFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0) & 
		TVM$VMWAREFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"93 COMPUTERS VMWARE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERSFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0) & TVM$TURNONFLAG > 0 & 
		(TVM$HOISTSFLAG > 0 | TVM$STATIONFLAG > 0 | TVM$SAWFLAG > 0 | TVM$TABLEFLAG > 0),"94 COMPUTERS POWERPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERSFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0) & 
		TVM$CONNECTFLAG > 0 & TVM$IPFLAG > 0 & TVM$USERSFLAG > 0,"95 COMPUTERS CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | 
		TVM$FREEZEFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$NOTSTARTFLAG > 0) & 
		(TVM$TABLEFLAG > 0 | TVM$ASSEMBLYFLAG > 0 | TVM$FRAMEFLAG > 0 | TVM$HINGINGFLAG > 0 | TVM$STATIONFLAG > 0 | TVM$BOXINGFLAG > 0 | TVM$HOISTFLAG > 0)
		,"96 SHARED COMP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0) & TVM$CONNECTFLAG > 0 & TVM$TREEFLAG == 0 & 
		(TVM$REPORTFLAG | TVM$BOXINGFLAG > 0 | TVM$HOISTFLAG > 0 | TVM$HINGINGFLAG > 0 | TVM$STATIONFLAG > 0 | TVM$WOODFLAG > 0 | TVM$WRAPFLAG > 0 | TVM$INSPECTIONFLAG > 0)
		,"97 SHARED COMP CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$CORDINGMACHINEFLAG > 0 & TVM$NOFLAG > 0 & TVM$SIGNALFLAG > 0,"98 CORD MACHINE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DASHBOARDFLAG > 0 & (TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0) & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"99 DASHBOARD CRASH",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DEALERLOCATORFLAG > 0 & TVM$NOTCOMINGUPFLAG > 0 & TVM$USERSFLAG > 0
		,"100 DEALERLOCATOR ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$DRIVESFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
 		  TVM$NOTACCESSFLAG > 0),"101 DRIVES DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$DRIVESFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0) & (TVM$ACCESSFLAG > 0 | TVM$CONNECTFLAG > 0) & TVM$TREEFLAG == 0),"102 DRIVES ACCESS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ECCFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
 		TVM$NOTACCESSFLAG > 0) & TVM$PHONEFLAG > 0,"103 ECC PHONE DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ECCFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
		 TVM$NOTACCESSFLAG > 0) & TVM$EQ1FLAG > 0,"104 ECC EQ1 DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ECCFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
 		TVM$NOTACCESSFLAG > 0) & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"105 ECC CRASH ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ECCFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0) & 
		TVM$CONFIRMATIONFLAG > 0,"106 ECC CONFIRM ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ECCFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		TVM$ORDERFLAG > 0 & TVM$CONFIRMATIONFLAG > 0 & TVM$FAILFLAG > 0,"107 ECC ORDER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$EP2FLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
 		TVM$SLOWFLAG > 0) & TVM$SYSTEMFLAG > 0 & TVM$SAPFLAG > 0,"108 EP2 SYSTEM SLOW",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$EQUIPMENTFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SYSTEMFLAG > 0
		,"109 SYSTEM EQUIP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$EVENTMANAGERFLAG > 0 & TVM$UNABLEFLAG > 0 & TVM$LOGINFLAG > 0 
			# & FABMAINT2FLAG > 0
		,"110 EVENT MANAGER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$FILEFLAG > 0 & TVM$SLOWFLAG > 0 & TVM$SAVEFLAG > 0),"111 FILE SAVE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$FIREWALLFLAG > 0 & TVM$MALFUNCTIONFLAG > 0,"112 FIREWALL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$HARDWAREFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 |
		 TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0),"113 HARDWARE DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STATIONFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
			TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		TVM$HARDWAREFLAG > 0,"114 STATION HDWARE PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$HDNAFLAG > 0 | TVM$HDGUESTFLAG > 0) & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0) & 
		TVM$CONNECTFLAG > 0 & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0) & TVM$WIRELESSFLAG > 0
		,"115 HDNA CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$HDNAFLAG > 0 | TVM$HDGUESTFLAG > 0) & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0) & 
		TVM$CONNECTFLAG > 0 & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"116 HDGUEST CONNECTPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$HOISTFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0) & 
		TVM$CONNECTFLAG > 0,"117 HOIST CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$ININFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
			 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$SLOWFLAG > 0) & TVM$SUPERVISORFLAG > 0
 			 & TVM$PASSWORDFLAG == 0),"118 ININ SLOW ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ININFLAG > 0 & (TVM$KICKEDOUTFLAG > 0 | TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 |
		 TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0) & 
		(TVM$TEAMFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"119 ININ DOWN USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ININFLAG > 0 & TVM$OUTFLAG > 0 & TVM$TEAMFLAG > 0 & TVM$RESETFLAG == 0 & TVM$SETUPFLAG == 0
		,"120 ININ OUT USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERNETFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		TVM$CONNECTFLAG > 0 & 
		(TVM$COMPUTERSFLAG > 0 | TVM$STATIONFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"121 INTERNET OUT USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$INTERNETFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		TVM$CONNECTFLAG > 0 & TVM$TREEFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$VPNFLAG == 0 & TVM$PASSWORDFLAG == 0 & TVM$MAILFLAG == 0)
		,"122 INTERNETCONNECTPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERNETFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
		 TVM$NOTACCESSFLAG > 0) & TVM$SITESFLAG > 0,"123 INTERNET SITES DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INTERNETFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		(TVM$SITEFLAG > 0 | TVM$SERVERFLAG > 0 | TVM$LINKFLAG> 0 | TVM$SAWFLAG > 0) & TVM$TREEFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$VPNFLAG == 0 & TVM$PASSWORDFLAG == 0 & TVM$MAILFLAG == 0
		,"124 INTERNET ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$INVOICESFLAG > 0 & TVM$NOTTRANSFERFLAG > 0 & TVM$EMAILFLAG > 0,"125 INVOICE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$JOBSFLAG > 0 & TVM$RUNFLAG > 0 & TVM$LONGFLAG > 0,"126 JOBS RUN LONG",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LATENCYFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		TVM$COMPUTERSFLAG > 0 & TVM$SLOWFLAG > 0,"127 LATENCY ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LATENCYFLAG > 0 & TVM$CALLFLAG > 0 & TVM$PHONEFLAG > 0,"128 LATENCY ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LINKFLAG> 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
 		TVM$NOTACCESSFLAG > 0) & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SITEFLAG == 0
		,"129 LINK DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LINKFLAG> 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | 
		TVM$NOTACCESSFLAG > 0) & TVM$SITEFLAG == 0 & TVM$TESTFLAG == 0 & TVM$DOCFLAG == 0,"130 LINK DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LINUXFLAG > 0 & TVM$PATCHFLAG > 0 & TVM$SECURITYFLAG > 0,"131 LINUX PATCH ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LITEFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
			 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & TVM$PICKFLAG > 0
		,"132 PICK LITE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LOGINFLAG > 0 & TVM$FAILFLAG > 0 & TVM$REPORTINGFLAG > 0 & TVM$ATTEMPTSFLAG > 0
		,"133 REPORT LOGON FAIL",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MAILFLAG > 0 & 
		(TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0) & 
		TVM$LISTENFLAG > 0 & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"134 MAIL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MAILFLAG > 0 & TVM$WITHOUTFLAG> 0 & TVM$ATTACHMENTFLAG > 0
		,"135 MAIL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MAILFLAG > 0 & TVM$MISSFLAG > 0 & TVM$FILEFLAG > 0 & TVM$ESNAFLAG > 0
		,"136 MAIL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0) & 
	TVM$TREEFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$VPNFLAG == 0 & TVM$PASSWORDFLAG == 0 & TVM$MAILFLAG == 0 & 
		TVM$NOVELLFLAG == 0 & TVM$LOGINFLAG == 0 & TVM$REMOTEFLAG == 0 & TVM$DRIVEFLAG == 0 & TVM$WIRELESSFLAG == 0,"137 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & 
		(TVM$ACCESSFLAG > 0 | TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0) & TVM$DIRECTWEBFLAG > 0 & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"138 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & 
		(TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | TVM$NOTABLEFLAG > 0) & 
		TVM$LOGINFLAG > 0 & TVM$WIFIFLAG > 0
		,"139 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & TVM$SLOWFLAG > 0 & TVM$DAYSFLAG > 0
		,"140 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$DENVERFLAG > 0
		,"141 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$NETWORKFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$WATERVLIETFLAG > 0
		,"142 NETWORK NOT CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ORDERSFLAG > 0 & TVM$ARENTFLAG > 0 & TVM$SYSTEMFLAG > 0 & 
		(TVM$TEAMFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"143 ORDERS USERS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PCFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
 		TVM$STOPPEDWORKFLAG > 0) & TVM$SILHOUETTEFLAG > 0 & TVM$INSPECTIONFLAG > 0
		,"144 INSPECTION PC ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$PCFLAG > 0 & TVM$ACCESSFLAG > 0 & TVM$ATTEMPFLAG > 0 & TVM$SOMEONEFLAG > 0)
		,"145 ACCESS ATTEMPTISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PCSFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0) & TVM$SEVERALFLAG > 0
		,"146 PCS CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PEDOMETERFLAG > 0 & TVM$NOTFLAG > 0 & TVM$SYNCFLAG > 0
		,"147 METER SYNC ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONEFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		TVM$DELAYFLAG > 0 & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"148 PHONE DELAY USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$PHONEFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0) & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0) & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$MAILFLAG == 0)
		,"149 PHONE CONNECT USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONEFLAG > 0 & TVM$DIALFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		TVM$FEWFLAG > 0 & TVM$CONSULTANTSFLAG > 0
		,"150 PHONE ISSUE USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONESFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		(
		#TVM$WEFLAG > 0 |
		TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0),"151 PHONES ISSUE USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONESFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SYSTEMFLAG > 0
		,"152 PHONES SYSTEM ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONEQUEUEFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"153 PHONE QUEUE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONESFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | 
		TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0),"154 PHONES DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$POWERFLAG > 0 & TVM$OUTAGEFLAG > 0,"155 POWER OUTAGE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PROCESSFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$CHAINFLAG > 0
		,"156 PROCESS CHAIN PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PRODUCTFLAG > 0 & TVM$INCORRECTFLAG > 0 & TVM$PAGEFLAG > 0 & TVM$INFOREQUESTFLAG > 0
		,"157 PRODUCT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PTSFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 |
		 TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0) & TVM$STATIONFLAG > 0
		,"158 PTS STATION DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RAILSAWFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 |
		  TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0),"159 RAILSAW DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$READSOFTFLAG > 0 & (TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | TVM$UNABLEFLAG > 0 |
		 TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"160 READSOFT USERS PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$READSOFTFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0)
		,"161 READSOFTCONNECTPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$READSOFTFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | 
			TVM$NOTACCESSFLAG > 0) & (TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"162 READSOFT DOWN USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$READSOFTFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0)
		,"163 READSOFT DOWN PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RECEIVERFLAG > 0 & TVM$BROKEFLAG > 0 & TVM$WIFIFLAG > 0,"164 WIFI RECEIVER BROKE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RECORDFLAG > 0 & TVM$OUTDATEDFLAG > 0 & TVM$INFOFLAG > 0,"165 RECORD OUTDATED",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$REMOTEDESKTOPFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$PRINTFLAG > 0 & 
		(TVM$INDIVIDUALSFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"166 REMOTE DESKTOPUSERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ROUTERFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
		 TVM$NOTACCESSFLAG > 0) ,"167 ROUTER DOWN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$ROUTERFLAG > 0 & TVM$SUPPLYFLAG > 0 & TVM$POWERFLAG > 0 & TVM$UNKNOWNFLAG > 0 & TVM$ININFLAG > 0
		,"168 ROUTER POWER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RUBYFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
		 TVM$STOPPEDWORKFLAG > 0) & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"169 RUBY DRIVE ACCESS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$RUBYFLAG > 0 & TVM$ACCESSFLAG > 0 & TVM$DRIVEFLAG > 0
		,"170 RUBY DRIVE ACCESS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAMBAFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | 
		TVM$STOPPEDWORKFLAG > 0) & TVM$SHAREFLAG > 0 & TVM$SAPFLAG > 0,"171 SAMBA ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAMBAFLAG > 0 & TVM$BROKEFLAG > 0 & TVM$SHAREFLAG > 0 & TVM$DEVFLAG > 0,"172 SAMBA BROKE ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAPFLAG > 0 & 
		(TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 |
		 TVM$NOTACCESSFLAG > 0) & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"173 SAP DOWN USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAPFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		TVM$RECEIVFLAG > 0 & TVM$GOODSFLAG > 0,"174 SAP GOODS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAWFLAG > 0 & TVM$NOTCUTFLAG > 0 & TVM$RIGHTFLAG > 0,"175 SAW CUT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAWFLAG > 0 & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"176 SAW CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAWFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$INTERNETFLAG > 0
		,"177 SAW INTERNET ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SAWFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
		 TVM$STOPPEDWORKFLAG > 0),"178 SAW NOT WORKING",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCOREBOARDFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0 | TVM$NOTALLOWFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0)
		,"179 SCOREBRDCONNECTPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCOREBOARDFLAG > 0 & 
		TVM$NOINTERNETFLAG > 0 & (TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0)
		,"180 SCOREBRDCONNECTINET",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCOREBOARDFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
		 TVM$STOPPEDWORKFLAG > 0 | 
		TVM$FREEZEFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$NOTSTARTFLAG > 0)
		,"181 SCOREBRD NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SENSORFLAG > 0 & TVM$FAILFLAG > 0,"182 SENSOR FAIL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$DELAYFLAG > 0 & TVM$TIMEFLAG > 0
		,"183 SERVER TIME DELAY",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$OUTFLAG > 0 & TVM$SYNCFLAG > 0
		,"184 SERVER OUT SYNC",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$DELAYFLAG > 0 & TVM$SYNCFLAG > 0
		,"185 SERVER DELAY SYNC",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$CHECKFLAG > 0 & TVM$HEALTHFLAG > 0
		,"186 SERVER CHECK HEALTH",paste(TVM$KEYWORDVAR))


TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & (TVM$REBOOTFLAG > 0 | TVM$REPLACEFLAG > 0 | TVM$RESETFLAG > 0 | TVM$RESTARTFLAG > 0)
		 & TVM$TREEFLAG == 0 & TVM$PASSWORDFLAG == 0 & TVM$NOVELLFLAG == 0 & TVM$LOGINFLAG == 0 & TVM$PCFLAG == 0 & TVM$COMPUTERFLAG == 0
		,"187 SERVER REBOOT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0 | TVM$NOTALLOWFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & TVM$TREEFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$VPNFLAG == 0 & TVM$PASSWORDFLAG == 0 & 
		TVM$NOVELLFLAG == 0 & TVM$LOGINFLAG == 0 & TVM$REMOTEFLAG == 0 & TVM$DRIVEFLAG == 0 & TVM$WIRELESSFLAG == 0 & TVM$PCFLAG == 0 & TVM$SOFTWAREFLAG == 0 & 
		TVM$REPORTFLAG == 0 & TVM$TABLETFLAG == 0 & TVM$IPADFLAG == 0 & TVM$COMPUTERFLAG == 0
		,"188 SERVER CONNECT PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$NOTFOUNDFLAG > 0 & TVM$NETWORKFLAG > 0 & TVM$TREEFLAG == 0
		,"189 SERVER NETWORK PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SECURITYFLAG > 0
		,"190 SERVER SECURITYPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$DAMAGFLAG > 0 & TVM$REPLACEFLAG > 0 & TVM$HARDDRIVEFLAG > 0
		,"191 SERVER DAMAGE PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 | TVM$NOTCOMMUNICATINGFLAG > 0 |
		 TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0) & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		 & TVM$TREEFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$VPNFLAG == 0 & TVM$PASSWORDFLAG == 0 & 
		TVM$NOVELLFLAG == 0 & TVM$LOGINFLAG == 0 & TVM$REMOTEFLAG == 0 & TVM$WIRELESSFLAG == 0 & TVM$SOFTWAREFLAG == 0 & TVM$TABLETFLAG == 0 & TVM$IPADFLAG == 0
		,"192 SERVER DOWN PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$LESSTHANFLAG > 0 & TVM$GBFLAG > 0 & TVM$FREEFLAG > 0
		,"193 SERVER SPACE PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$SLOWFLAG > 0 
# & TVM$TXMAPD01FLAG > 0 
		,"194 SERVER SLOW PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SILKFLAG > 0 & TVM$SLOWFLAG > 0 & TVM$DATABASEFLAG > 0
		,"195 SILK SLOW ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SILKFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
			 & TVM$WECANTLOGINFLAG > 0 ,"196 SILK USERS CONNECT",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SILKFLAG > 0 & 
		(TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 |
		 TVM$STOPPEDWORKFLAG > 0 | 
		TVM$FREEZEFLAG > 0 | TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0 | TVM$NOTSTARTFLAG > 0) & 
		(TVM$AREFLAG > 0 | TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0)
		,"197 SILK USERS NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SILKFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & TVM$TREEFLAG == 0 & TVM$PASSWORDFLAG == 0 & 
		TVM$NOVELLFLAG == 0 & TVM$LOGINFLAG == 0,"198 SILK CONNECT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STATIONFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		 & TVM$ALLFLAG > 0 & TVM$VOLUMEFLAG == 0 & TVM$INSTALLFLAG == 0,"199 STATIONS ALL PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STATIONFLAG > 0 & TVM$SURGEFLAG > 0 & TVM$POWERFLAG > 0 & (TVM$DOWNFLAG > 0 | TVM$CRASHFLAG > 0 | TVM$FAILFLAG > 0 | TVM$NOTRESPONDFLAG > 0 |
			 TVM$NOTCOMMUNICATINGFLAG > 0 | TVM$NOTFUNCTIONFLAG > 0 | TVM$NOTACCESSFLAG > 0)
		,"200 STATION DOWN PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SWITCHFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$COMPUTERFLAG == 0 & TVM$LAPTOPFLAG == 0 & TVM$PASSWORDFLAG == 0 & 
		TVM$NOVELLFLAG == 0 & TVM$SOFTWAREFLAG == 0 & TVM$TABLETFLAG == 0 & TVM$IPADFLAG == 0 & TVM$PCFLAG == 0 & TVM$INSTALLFLAG == 0 & TVM$PHONEFLAG == 0 & TVM$ORDERFLAG == 0 & TVM$INTERNETFLAG == 0
		,"201 SWITCH ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SWITCHFLAG > 0 & TVM$DISABLEFLAG > 0 & TVM$PORTSFLAG > 0 & TVM$STATUSFLAG > 0,
                         "202 SWITCH DISABLEPORTS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SYSTEMFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & TVM$SERVERFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0)
		,"203 SYSTEM SERVER ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TEAMCENTERFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$SAVFLAG > 0 & 
		(TVM$WEFLAG > 0 | TVM$USERSFLAG > 0 | TVM$EVERYBODYFLAG > 0) ,"204 TEAMCENTERSAVEUSERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TIGERSTOPFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & TVM$TABLEFLAG > 0
		,"205 TIGERSTOP NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TOUCHSCREENFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | TVM$LOSFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0 | TVM$NOTALLOWFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & (TVM$TABLEFLAG > 0 | TVM$STATIONFLAG > 0)
		,"206 TOUCHSCREEN ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TOUCHSCREENFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		(TVM$TABLEFLAG > 0 | TVM$STATIONFLAG > 0 
		#| TVM$FRAMEFLAG > 0 | TVM$LASERFLAG > 0
		),"207 TOUCHSCREEN NOTWORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$TOUCHSCREENFLAG > 0 & TVM$DAMAGEFLAG > 0 & 
		(TVM$TABLEFLAG > 0 | TVM$STATIONFLAG > 0)
		,"208 TOUCHSCREEN DAMAGE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$VIGNETTEFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		TVM$POWERFLAG > 0 & TVM$COMPUTERFLAG > 0,"209 VIGNETTE NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$VMWAREFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$COMPUTERSFLAG > 0
		,"210 VMWARE ISSUE USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$VOICEMAILFLAG > 0 & 
		(TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$ESNAFLAG > 0
		,"211 VOICEMAIL ESNA PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WEBSITEFLAG > 0 & TVM$DOWNFLAG > 0 & TVM$BULLETINFLAG > 0
		,"212 SITE BULLETIN DOWN",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WIFIFLAG > 0 & TVM$NOTAUTHENTICATEFLAG > 0 & TVM$CORRECTFLAG > 0
		,"213 WIFI ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WOODFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		TVM$COMPUTERFLAG > 0 & TVM$PULLFLAG > 0 ,"214 WOODPULLCOMPNOTWORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WOODFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & TVM$HOISTFLAG > 0
		,"215 WOOD HOIST NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$WYSETERMINALSFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$DHCPFLAG > 0 & 
		 TVM$FEWFLAG > 0,"216 WYSETERMINALS ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$ZENWORKSFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0 | TVM$LOSFLAG > 0 | 
		TVM$LOSTFLAG > 0 | TVM$DROPFLAG > 0 | TVM$LOOSFLAG > 0 | TVM$LIMITFLAG > 0 | TVM$NOTALLOWFLAG > 0) & 
		(TVM$CONNECTFLAG > 0 | TVM$COMMUNICATIONFLAG > 0 | TVM$ACCESSFLAG > 0) & TVM$PASSWORDFLAG == 0)
		,"217 ZENWORKSCONNECTPROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MONITORFLAG > 0 & TVM$FREEZFLAG > 0 & (TVM$TABLEFLAG > 0 | TVM$HOISTFLAG > 0 | TVM$STATIONFLAG > 0)
		,"218 MONITOR FREEZSHARED",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$COMPUTERFLAG > 0 & 
		(TVM$WONTFLAG > 0 | TVM$DOESNTFLAG > 0 | TVM$DOESNOTFLAG > 0 | TVM$CANNOTFLAG > 0 | 
		TVM$UNABLEFLAG > 0 | TVM$UNABLEFLAG > 0 | TVM$UNABLLEFLAG > 0 | TVM$UBABLEFLAG > 0 | 
		TVM$NOTABLEFLAG > 0 | TVM$WILLNOTFLAG > 0 | TVM$CANTFLAG > 0 | TVM$FAILFLAG > 0) & 
		TVM$RAILFLAG > 0 & TVM$CUTFLAG > 0,"219 RAIL COMP ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$LABELFLAG > 0 & (TVM$PROBLEMFLAG > 0 | TVM$ISSUEFLAG > 0 | TVM$ERRORFLAG > 0 | TVM$TROUBLEFLAG > 0) & TVM$FABRICFLAG > 0
		,"220 LABEL ISSUE FABRIC",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$STATIONFLAG > 0 & TVM$NOTGIVFLAG > 0 & TVM$LABELFLAG > 0
		,"221 STATION LABEL ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$KITACEFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
			 TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		TVM$CARTFLAG > 0,"222 KITACE NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SERVERFLAG > 0 & TVM$REBOOTFLAG > 0 & TVM$USERSFLAG > 0
		,"223 SERVER REBOOT USERS",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse ((TVM$ZEBRAFLAG > 0 | TVM$PRINTFLAG > 0) & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 |
		  TVM$ISNTWORKFLAG > 0 | TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0) & 
		  TVM$UPSFLAG > 0,"224 UPS PRINT NOT WORK",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$KRONOSFLAG > 0 & TVM$SLOWFLAG > 0,"225 KRONOS SLOW",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$PHONEFLAG > 0 & TVM$PROBLEMFLAG > 0 & TVM$SYSTEMFLAG > 0
		,"226 PHONE SYSTEM PROB",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$MACHINEFLAG > 0 & TVM$LINEFLAG > 0 & TVM$LABELFLAG > 0
		,"227 LABEL PRINT ISSUE",paste(TVM$KEYWORDVAR))

TVM$KEYWORDVAR <- ifelse (TVM$SCANFLAG > 0 & (TVM$NOTWORKFLAG > 0 | TVM$DOESNOTWORKFLAG > 0 | TVM$DOESNTWORKFLAG > 0 | TVM$ISNTWORKFLAG > 0 |
		  TVM$QUITWORKFLAG > 0 | TVM$WONTWORKFLAG > 0 | TVM$STOPPEDWORKFLAG > 0),"228 SCAN NOT WORK",paste(TVM$KEYWORDVAR))

TVM$CONFIDENCE	<-	ifelse (TVM$KEYWORDVAR=="1 PRINT LABEL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="2 PRINT ZEBRA ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="3 PRINT WASP ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="4 PRINT WRAP ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="5 PRINT WOOD ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="6 PRINT STATION ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="7 PRINT KAIZEN ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="8 PRINT FRAME ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="9 PRINT ALTER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="10 PRINT ATS ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="11 PRINT BOXING ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="12 PRINT SAP ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="13 PRINT SAW ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="14 PRINT STILE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="15 PRINT BARCODE ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="16 PRINT PACKLIST ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="17 PRINT TONER ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="18 PRINT INK ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="19 PRINT FUSER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="20 PRINT RIBBON ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="21 PRINT ROLLER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="22 PRINT PRODUCTON ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="23 PRINT COPIER ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="24 PRINT EHARDWARE ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="25 PRINT ORDER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="26 PRINT SHIP ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="27 PRINT MRKT ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="28 PRINT PARTS ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="29 PRINT PACK ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="30 FAX MACHINE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="31 SCAN STATION ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="32 SCAN TABLE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="33 SCAN KIOSK ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="34 SCAN CART ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="35 SCAN HOIST ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="36 SCAN ASSEMBLY ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="37 SCAN KITACE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="38 SCAN BARCODE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="39 SCAN PARTS ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="40 SCAN FABRIC ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="41 SCAN EHDWARE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="42 SCAN SAW ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="43 SCAN PACK ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="44 SCAN SORT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="45 SCAN SYSTEM ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="46 SCAN SCREEN ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="47 SCAN FAUX ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="48 SCAN KAIZEN ISSUE","85%",paste(TVM$CONFIDENCE)))))))))))))))))))))))))))))))))))))))))))))))))
		
		
TVM$CONFIDENCE	<-	ifelse (TVM$KEYWORDVAR=="49 SCAN COMPUTER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="50 SCAN CONNECT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="51 SCAN ORDER ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="52 SCAN PRODTIME ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="53 SCAN NOSCAN ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="54 SCAN BARCODE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="55 DWEB SERVICE ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="56 DWEB ACCESS ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="57 DWEB ACCESS ISSUE2","80%",
		ifelse (TVM$KEYWORDVAR=="58 DWEB SLOW ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="59 DWEB USERS ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="60 DWEB NOT WORK ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="61 DWEB SERVER ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="62 DWEB SITE ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="63 DWEB DOWN ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="64 CLOCK DOWN ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="65 CLOCK PROB ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="66 KRONOS USERS ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="67 KRONOS ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="68 INTERFACE DOWN","90%",
		ifelse (TVM$KEYWORDVAR=="69 INTERFACE TRAFFIC","85%",
		ifelse (TVM$KEYWORDVAR=="70 INTERFACE FLUCUATED","85%",
		ifelse (TVM$KEYWORDVAR=="71 INTERFACE OUTAGE","85%",
		ifelse (TVM$KEYWORDVAR=="72 NODE DOWN MONITOR","85%",
		ifelse (TVM$KEYWORDVAR=="73 NODE UP MONITOR","85%",
		ifelse (TVM$KEYWORDVAR=="74 NODE UP MONITOR","85%",
		ifelse (TVM$KEYWORDVAR=="75 HIGH PACKET LOSS","90%",
		ifelse (TVM$KEYWORDVAR=="76 XML MONITOR","80%",
		ifelse (TVM$KEYWORDVAR=="77 FS ALERT","90%",
		ifelse (TVM$KEYWORDVAR=="78 NETWORK DOWNMONITOR","90%",
		ifelse (TVM$KEYWORDVAR=="79 NETWORK MONITOR","85%",
		ifelse (TVM$KEYWORDVAR=="80 TRAFFIC MONITOR","80%",
		ifelse (TVM$KEYWORDVAR=="81 ADAPTERS ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="82 ATS DOWNUSERS ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="83 ATS USERS ERROR","80%",
		ifelse (TVM$KEYWORDVAR=="84 ATS SERVER ERROR","85%",
		ifelse (TVM$KEYWORDVAR=="85 ATS STORAGE ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="86 ATS ORDER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="87 BOXINGCONNECT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="88 BLDG NO POWER ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="89 CALLCENTER NO CALL","95%",
		ifelse (TVM$KEYWORDVAR=="90 CALL MISS DATA","85%",
		ifelse (TVM$KEYWORDVAR=="91 CALL QUEUE ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="92 COMPUTERS DOWN","90%",
		ifelse (TVM$KEYWORDVAR=="93 COMPUTERS VMWARE","90%",
		ifelse (TVM$KEYWORDVAR=="94 COMPUTERS POWERPROB","85%",
		ifelse (TVM$KEYWORDVAR=="95 COMPUTERS CONNECT","80%",
		ifelse (TVM$KEYWORDVAR=="96 SHARED COMP ISSUE","90%",paste(TVM$CONFIDENCE)))))))))))))))))))))))))))))))))))))))))))))))))
		
		
TVM$CONFIDENCE	<-	ifelse (TVM$KEYWORDVAR=="97 SHARED COMP CONNECT","85%",
		ifelse (TVM$KEYWORDVAR=="98 CORD MACHINE ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="99 DASHBOARD CRASH","85%",
		ifelse (TVM$KEYWORDVAR=="100 DEALERLOCATOR ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="101 DRIVES DOWN ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="102 DRIVES ACCESS ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="103 ECC PHONE DOWN","95%",
		ifelse (TVM$KEYWORDVAR=="104 ECC EQ1 DOWN","80%",
		ifelse (TVM$KEYWORDVAR=="105 ECC CRASH ISSUE","75%",
		ifelse (TVM$KEYWORDVAR=="106 ECC CONFIRM ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="107 ECC ORDER ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="108 EP2 SYSTEM SLOW","85%",
		ifelse (TVM$KEYWORDVAR=="109 SYSTEM EQUIP ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="110 EVENT MANAGER ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="111 FILE SAVE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="112 FIREWALL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="113 HARDWARE DOWN","85%",
		ifelse (TVM$KEYWORDVAR=="114 STATION HDWARE PROB","85%",
		ifelse (TVM$KEYWORDVAR=="115 HDNA CONNECT ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="116 HDGUEST CONNECTPROB","80%",
		ifelse (TVM$KEYWORDVAR=="117 HOIST CONNECT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="118 ININ SLOW ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="119 ININ DOWN USERS","85%",
		ifelse (TVM$KEYWORDVAR=="120 ININ OUT USERS","75%",
		ifelse (TVM$KEYWORDVAR=="121 INTERNET OUT USERS","80%",
		ifelse (TVM$KEYWORDVAR=="122 INTERNETCONNECTPROB","75%",
		ifelse (TVM$KEYWORDVAR=="123 INTERNET SITES DOWN","95%",
		ifelse (TVM$KEYWORDVAR=="124 INTERNET ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="125 INVOICE ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="126 JOBS RUN LONG","85%",
		ifelse (TVM$KEYWORDVAR=="127 LATENCY ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="128 LATENCY ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="129 LINK DOWN ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="130 LINK DOWN ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="131 LINUX PATCH ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="132 PICK LITE ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="133 REPORT LOGON FAIL","90%",
		ifelse (TVM$KEYWORDVAR=="134 MAIL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="135 MAIL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="136 MAIL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="137 NETWORK NOT CONNECT","80%",
		ifelse (TVM$KEYWORDVAR=="138 NETWORK NOT CONNECT","80%",
		ifelse (TVM$KEYWORDVAR=="139 NETWORK NOT CONNECT","75%",
		ifelse (TVM$KEYWORDVAR=="140 NETWORK NOT CONNECT","85%",
		ifelse (TVM$KEYWORDVAR=="141 NETWORK NOT CONNECT","95%",
		ifelse (TVM$KEYWORDVAR=="142 NETWORK NOT CONNECT","95%",
		ifelse (TVM$KEYWORDVAR=="143 ORDERS USERS ISSUE","85%",paste(TVM$CONFIDENCE))))))))))))))))))))))))))))))))))))))))))))))))
		
		
TVM$CONFIDENCE	<-	ifelse (TVM$KEYWORDVAR=="144 INSPECTION PC ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="145 ACCESS ATTEMPTISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="146 PCS CONNECT ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="147 METER SYNC ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="148 PHONE DELAY USERS","90%",
		ifelse (TVM$KEYWORDVAR=="149 PHONE CONNECT USERS","90%",
		ifelse (TVM$KEYWORDVAR=="150 PHONE ISSUE USERS","90%",
		ifelse (TVM$KEYWORDVAR=="151 PHONES ISSUE USERS","90%",
		ifelse (TVM$KEYWORDVAR=="152 PHONES SYSTEM ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="153 PHONE QUEUE ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="154 PHONES DOWN ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="155 POWER OUTAGE","85%",
		ifelse (TVM$KEYWORDVAR=="156 PROCESS CHAIN PROB","85%",
		ifelse (TVM$KEYWORDVAR=="157 PRODUCT ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="158 PTS STATION DOWN","85%",
		ifelse (TVM$KEYWORDVAR=="159 RAILSAW DOWN","85%",
		ifelse (TVM$KEYWORDVAR=="160 READSOFT USERS PROB","80%",
		ifelse (TVM$KEYWORDVAR=="161 READSOFTCONNECTPROB","75%",
		ifelse (TVM$KEYWORDVAR=="162 READSOFT DOWN USERS","85%",
		ifelse (TVM$KEYWORDVAR=="163 READSOFT DOWN PROB","80%",
		ifelse (TVM$KEYWORDVAR=="164 WIFI RECEIVER BROKE","85%",
		ifelse (TVM$KEYWORDVAR=="165 RECORD OUTDATED","80%",
		ifelse (TVM$KEYWORDVAR=="166 REMOTE DESKTOPUSERS","80%",
		ifelse (TVM$KEYWORDVAR=="167 ROUTER DOWN ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="168 ROUTER POWER ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="169 RUBY DRIVE ACCESS","90%",
		ifelse (TVM$KEYWORDVAR=="170 RUBY DRIVE ACCESS","90%",
		ifelse (TVM$KEYWORDVAR=="171 SAMBA ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="172 SAMBA BROKE ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="173 SAP DOWN USERS","90%",
		ifelse (TVM$KEYWORDVAR=="174 SAP GOODS ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="175 SAW CUT ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="176 SAW CONNECT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="177 SAW INTERNET ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="178 SAW NOT WORKING","85%",
		ifelse (TVM$KEYWORDVAR=="179 SCOREBRDCONNECTPROB","80%",
		ifelse (TVM$KEYWORDVAR=="180 SCOREBRDCONNECTINET","80%",
		ifelse (TVM$KEYWORDVAR=="181 SCOREBRD NOT WORK","85%",
		ifelse (TVM$KEYWORDVAR=="182 SENSOR FAIL ISSUE","95%",
		ifelse (TVM$KEYWORDVAR=="183 SERVER TIME DELAY","85%",
		ifelse (TVM$KEYWORDVAR=="184 SERVER OUT SYNC","85%",
		ifelse (TVM$KEYWORDVAR=="185 SERVER DELAY SYNC","85%",
		ifelse (TVM$KEYWORDVAR=="186 SERVER CHECK HEALTH","85%",
		ifelse (TVM$KEYWORDVAR=="187 SERVER REBOOT","90%",
		ifelse (TVM$KEYWORDVAR=="188 SERVER CONNECT PROB","90%",
		ifelse (TVM$KEYWORDVAR=="189 SERVER NETWORK PROB","90%",
		ifelse (TVM$KEYWORDVAR=="190 SERVER SECURITYPROB","90%",paste(TVM$CONFIDENCE))))))))))))))))))))))))))))))))))))))))))))))))
		
		
TVM$CONFIDENCE	<-	ifelse (TVM$KEYWORDVAR=="191 SERVER DAMAGE PROB","90%",
		ifelse (TVM$KEYWORDVAR=="192 SERVER DOWN PROB","90%",
		ifelse (TVM$KEYWORDVAR=="193 SERVER SPACE PROB","90%",
		ifelse (TVM$KEYWORDVAR=="194 SERVER SLOW PROB","90%",
		ifelse (TVM$KEYWORDVAR=="195 SILK SLOW ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="196 SILK USERS CONNECT","90%",
		ifelse (TVM$KEYWORDVAR=="197 SILK USERS NOT WORK","90%",
		ifelse (TVM$KEYWORDVAR=="198 SILK CONNECT ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="199 STATIONS ALL PROB","85%",
		ifelse (TVM$KEYWORDVAR=="200 STATION DOWN PROB","90%",
		ifelse (TVM$KEYWORDVAR=="201 SWITCH ISSUE","80%",
		ifelse (TVM$KEYWORDVAR=="202 SWITCH DISABLEPORTS","85%",
		ifelse (TVM$KEYWORDVAR=="203 SYSTEM SERVER ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="204 TEAMCENTERSAVEUSERS","85%",
		ifelse (TVM$KEYWORDVAR=="205 TIGERSTOP NOT WORK","85%",
		ifelse (TVM$KEYWORDVAR=="206 TOUCHSCREEN ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="207 TOUCHSCREEN NOTWORK","85%",
		ifelse (TVM$KEYWORDVAR=="208 TOUCHSCREEN DAMAGE","85%",
		ifelse (TVM$KEYWORDVAR=="209 VIGNETTE NOT WORK","80%",
		ifelse (TVM$KEYWORDVAR=="210 VMWARE ISSUE USERS","90%",
		ifelse (TVM$KEYWORDVAR=="211 VOICEMAIL ESNA PROB","90%",
		ifelse (TVM$KEYWORDVAR=="212 SITE BULLETIN DOWN","80%",
		ifelse (TVM$KEYWORDVAR=="213 WIFI ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="214 WOODPULLCOMPNOTWORK","80%",
		ifelse (TVM$KEYWORDVAR=="215 WOOD HOIST NOT WORK","80%",
		ifelse (TVM$KEYWORDVAR=="216 WYSETERMINALS ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="217 ZENWORKSCONNECTPROB","85%",
		ifelse (TVM$KEYWORDVAR=="218 MONITOR FREEZSHARED","85%",
		ifelse (TVM$KEYWORDVAR=="219 RAIL COMP ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="220 LABEL ISSUE FABRIC","90%",
		ifelse (TVM$KEYWORDVAR=="221 STATION LABEL ISSUE","90%",
		ifelse (TVM$KEYWORDVAR=="222 KITACE NOT WORK","85%",
		ifelse (TVM$KEYWORDVAR=="223 SERVER REBOOT USERS","90%",
		ifelse (TVM$KEYWORDVAR=="224 UPS PRINT NOT WORK","90%",
		ifelse (TVM$KEYWORDVAR=="225 KRONOS SLOW","85%",
		ifelse (TVM$KEYWORDVAR=="226 PHONE SYSTEM PROB","90%",
		ifelse (TVM$KEYWORDVAR=="227 LABEL PRINT ISSUE","85%",
		ifelse (TVM$KEYWORDVAR=="228 SCAN NOT WORK","75%",paste(TVM$CONFIDENCE)))))))))))))))))))))))))))))))))))))))

setwd("D:/R Output files")
write.csv(TVM,file="TVM_Output.csv")
