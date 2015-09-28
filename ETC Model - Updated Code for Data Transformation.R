
## Reading the file ##

ETC <- read.csv(file="D:/R Input files/AEETCTRAINING-08242015.csv",header = T)

## Subject column - Removing punctuations ##

library("tm")
library("tmap")
ETC$Subject_Fin       <- toupper(ETC$Subject)
ETC$Subject_Fin       <- removePunctuation(ETC$Subject_Fin) ##This remove %^&*()~!@#${}_+:"<>?,./;'[]-= except Â ##
ETC$Subject_Fin       <- stripWhitespace(ETC$Subject_Fin)
##ETC$Subject       <- removeWords(ETC$Subject,c("AE","COST"))   #To remove specific words from the phrase#

## If else statement in Subject column ##

ETC$ABLEFLAG<-ifelse (grepl("ABLE",ETC$Subject_Fin),1,0)
ETC$ACCESSFLAG<-ifelse (grepl("ACCESS",ETC$Subject_Fin),1,0)
ETC$ACCOUNTFLAG<-ifelse (grepl("ACCOUNT",ETC$Subject_Fin),1,0)
ETC$ACROBATFLAG<-ifelse (grepl("ACROBAT",ETC$Subject_Fin),1,0)
ETC$ACTIVATEFLAG<-ifelse (grepl("ACTIVATE",ETC$Subject_Fin),1,0)
ETC$ACUTECAREFLAG<-ifelse (grepl("ACUTE CARE",ETC$Subject_Fin),1,0)
ETC$ADAPTERSFLAG<-ifelse (grepl("ADAPTERS",ETC$Subject_Fin),1,0)
ETC$ADDFLAG<-ifelse (grepl("ADD",ETC$Subject_Fin),1,0)
ETC$ADDRESSFLAG<-ifelse (grepl("ADDRESS",ETC$Subject_Fin),1,0)
ETC$ADMINFLAG<-ifelse (grepl("ADMIN",ETC$Subject_Fin),1,0)
ETC$ADOBEFLAG<-ifelse (grepl("ADOBE",ETC$Subject_Fin),1,0)
ETC$ADPFLAG<-ifelse (grepl("ADP",ETC$Subject_Fin),1,0)
ETC$AEFLAG<-ifelse (grepl("AE",ETC$Subject_Fin),1,0)
ETC$AGENTIDFLAG<-ifelse (grepl("AGENT ID",ETC$Subject_Fin),1,0)
ETC$AIRSFLAG<-ifelse (grepl("AIRS",ETC$Subject_Fin),1,0)
ETC$AIRWATCHFLAG<-ifelse (grepl("AIRWATCH",ETC$Subject_Fin),1,0)
ETC$ALERTFLAG<-ifelse (grepl("ALERT",ETC$Subject_Fin),1,0)
ETC$ALLFLAG<-ifelse (grepl("ALL",ETC$Subject_Fin),1,0)
ETC$ALLANFLAG<-ifelse (grepl("ALLAN",ETC$Subject_Fin),1,0)
ETC$ALLOWFLAG<-ifelse (grepl("ALLOW",ETC$Subject_Fin),1,0)
ETC$ALLSCRIPTSFLAG<-ifelse (grepl("ALLSCRIPTS",ETC$Subject_Fin),1,0)
ETC$ALLUSERFLAG<-ifelse (grepl("ALL USER",ETC$Subject_Fin),1,0)
ETC$ALTERFLAG<-ifelse (grepl("ALTER",ETC$Subject_Fin),1,0)
ETC$ALTIUMFLAG<-ifelse (grepl("ALTIUM",ETC$Subject_Fin),1,0)
ETC$ANONYMOUSFLAG<-ifelse (grepl("ANONYMOUS",ETC$Subject_Fin),1,0)
ETC$APACHEFLAG<-ifelse (grepl("APACHE",ETC$Subject_Fin),1,0)
ETC$APPFLAG<-ifelse (grepl("APP",ETC$Subject_Fin),1,0)
ETC$APPLICATIONFLAG<-ifelse (grepl("APPLICATION",ETC$Subject_Fin),1,0)
ETC$APPOINTMENTFLAG<-ifelse (grepl("APPOINTMENT",ETC$Subject_Fin),1,0)
ETC$APPROVFLAG<-ifelse (grepl("APPROV",ETC$Subject_Fin),1,0)
ETC$APPROVALFLAG<-ifelse (grepl("APPROVAL",ETC$Subject_Fin),1,0)
ETC$AREFLAG<-ifelse (grepl("ARE",ETC$Subject_Fin),1,0)
ETC$AREAFLAG<-ifelse (grepl("AREA",ETC$Subject_Fin),1,0)
ETC$ARENTFLAG<-ifelse (grepl("ARENT",ETC$Subject_Fin),1,0)
ETC$AS400FLAG<-ifelse (grepl("AS400",ETC$Subject_Fin),1,0)
ETC$ASAPFLAG<-ifelse (grepl("ASAP",ETC$Subject_Fin),1,0)
ETC$ASSEMBLYFLAG<-ifelse (grepl("ASSEMBLY",ETC$Subject_Fin),1,0)
ETC$ASSETFLAG<-ifelse (grepl("ASSET",ETC$Subject_Fin),1,0)
ETC$ASTEAFLAG<-ifelse (grepl("ASTEA",ETC$Subject_Fin),1,0)
ETC$ATSFLAG<-ifelse (grepl("ATS",ETC$Subject_Fin),1,0)
ETC$ATTACHMENTFLAG<-ifelse (grepl("ATTACHMENT",ETC$Subject_Fin),1,0)
ETC$ATTACKFLAG<-ifelse (grepl("ATTACK",ETC$Subject_Fin),1,0)
ETC$ATTEMPFLAG<-ifelse (grepl("ATTEMP",ETC$Subject_Fin),1,0)
ETC$ATTEMPTSFLAG<-ifelse (grepl("ATTEMPTS",ETC$Subject_Fin),1,0)
ETC$AUTHFLAG<-ifelse (grepl("AUTH",ETC$Subject_Fin),1,0)
ETC$AUTHENTICATEFLAG<-ifelse (grepl("AUTHENTICATE",ETC$Subject_Fin),1,0)
ETC$AUTHORIZATIONFLAG<-ifelse (grepl("AUTHORIZATION",ETC$Subject_Fin),1,0)
ETC$AUTHORIZEFLAG<-ifelse (grepl("AUTHORIZE",ETC$Subject_Fin),1,0)
ETC$AUTHORFLAG<-ifelse (grepl("AUTHOR",ETC$Subject_Fin),1,0)
ETC$AUTOFLAG<-ifelse (grepl("AUTO",ETC$Subject_Fin),1,0)
ETC$AVAILABLEFLAG<-ifelse (grepl("AVAILABLE",ETC$Subject_Fin),1,0)
ETC$AVAYAFLAG<-ifelse (grepl("AVAYA",ETC$Subject_Fin),1,0)
ETC$AVOIDFLAG<-ifelse (grepl("AVOID",ETC$Subject_Fin),1,0)
ETC$BACKINFLAG<-ifelse (grepl("BACKIN",ETC$Subject_Fin),1,0)
ETC$BACKINGFLAG<-ifelse (grepl("BACKING",ETC$Subject_Fin),1,0)
ETC$BACKINGUPFLAG<-ifelse (grepl("BACKINGUP",ETC$Subject_Fin),1,0)
ETC$BACKUPFLAG<-ifelse (grepl("BACKUP",ETC$Subject_Fin),1,0)
ETC$BADFLAG<-ifelse (grepl("BAD",ETC$Subject_Fin),1,0)
ETC$BARCODEFLAG<-ifelse (grepl("BAR CODE",ETC$Subject_Fin),1,0)
ETC$BARCODEFLAG2<-ifelse (grepl("BARCODE",ETC$Subject_Fin),1,0)
ETC$BASEFLAG<-ifelse (grepl("BASE",ETC$Subject_Fin),1,0)
ETC$BATTERYFLAG<-ifelse (grepl("BATTERY",ETC$Subject_Fin),1,0)
ETC$BEEPFLAG<-ifelse (grepl("BEEP",ETC$Subject_Fin),1,0)
ETC$BILLFLAG<-ifelse (grepl("BILL",ETC$Subject_Fin),1,0)
ETC$BINSFLAG<-ifelse (grepl("BINS",ETC$Subject_Fin),1,0)
ETC$BLANKFLAG<-ifelse (grepl("BLANK",ETC$Subject_Fin),1,0)
ETC$BLOCKFLAG<-ifelse (grepl("BLOCK",ETC$Subject_Fin),1,0)
ETC$BLURFLAG<-ifelse (grepl("BLUR",ETC$Subject_Fin),1,0)
ETC$BOMFLAG<-ifelse (grepl("BOM",ETC$Subject_Fin),1,0)
ETC$BOOKFLAG<-ifelse (grepl("BOOK",ETC$Subject_Fin),1,0)
ETC$BOOTINGFLAG<-ifelse (grepl("BOOTING",ETC$Subject_Fin),1,0)
ETC$BOXFLAG<-ifelse (grepl("BOX",ETC$Subject_Fin),1,0)
ETC$BOXINGFLAG<-ifelse (grepl("BOXING",ETC$Subject_Fin),1,0)
ETC$BROKEFLAG<-ifelse (grepl("BROKE",ETC$Subject_Fin),1,0)
ETC$BROKENFLAG<-ifelse (grepl("BROKEN",ETC$Subject_Fin),1,0)
ETC$BROOMFIELDFLAG<-ifelse (grepl("BROOMFIELD",ETC$Subject_Fin),1,0)
ETC$BUGFLAG<-ifelse (grepl("BUG",ETC$Subject_Fin),1,0)
ETC$BUILDINGFLAG<-ifelse (grepl("BUILDING",ETC$Subject_Fin),1,0)
ETC$BULLETINFLAG<-ifelse (grepl("BULLETIN",ETC$Subject_Fin),1,0)
ETC$BWFLAG<-ifelse (grepl("BW",ETC$Subject_Fin),1,0)
ETC$CALENDARFLAG<-ifelse (grepl("CALENDAR",ETC$Subject_Fin),1,0)
ETC$CABLEFLAG<-ifelse (grepl("CABLE",ETC$Subject_Fin),1,0)
ETC$CADFLAG<-ifelse (grepl("CAD",ETC$Subject_Fin),1,0)
ETC$CALIBRATFLAG<-ifelse (grepl("CALIBRAT",ETC$Subject_Fin),1,0)
ETC$CALLCENTERFLAG<-ifelse (grepl("CALL CENTER",ETC$Subject_Fin),1,0)
ETC$CALLFLAG<-ifelse (grepl("CALL",ETC$Subject_Fin),1,0)
ETC$CANCELFLAG<-ifelse (grepl("CANCEL",ETC$Subject_Fin),1,0)
ETC$CANNOTFLAG<-ifelse (grepl("CANNOT",ETC$Subject_Fin),1,0)
ETC$CANNOTFLAG2<-ifelse (grepl("CAN NOT",ETC$Subject_Fin),1,0)
ETC$CANNOTGETFLAG<-ifelse (grepl("CAN NOT GET",ETC$Subject_Fin),1,0)
ETC$CANTFLAG<-ifelse (grepl("CANT",ETC$Subject_Fin),1,0)
ETC$CANTGETFLAG<-ifelse (grepl("CANT GET",ETC$Subject_Fin),1,0)
ETC$CARFLAG<-ifelse (grepl("CAR",ETC$Subject_Fin),1,0)
ETC$CARTFLAG<-ifelse (grepl("CART",ETC$Subject_Fin),1,0)
ETC$CARTRIDGEFLAG<-ifelse (grepl("CARTRIDGE",ETC$Subject_Fin),1,0)
ETC$CASEFLAG<-ifelse (grepl("CASE",ETC$Subject_Fin),1,0)
ETC$CENSUSFLAG<-ifelse (grepl("CENSUS",ETC$Subject_Fin),1,0)
ETC$CERNERFLAG<-ifelse (grepl("CERNER",ETC$Subject_Fin),1,0)
ETC$CHAINFLAG<-ifelse (grepl("CHAIN",ETC$Subject_Fin),1,0)
ETC$CHANGEFLAG<-ifelse (grepl("CHANGE",ETC$Subject_Fin),1,0)
ETC$CHARGFLAG<-ifelse (grepl("CHARG",ETC$Subject_Fin),1,0)
ETC$CHARTFLAG<-ifelse (grepl("CHART",ETC$Subject_Fin),1,0)
ETC$CHECKFLAG<-ifelse (grepl("CHECK",ETC$Subject_Fin),1,0)
ETC$CHINESEFLAG<-ifelse (grepl("CHINESE",ETC$Subject_Fin),1,0)
ETC$CISCOFLAG<-ifelse (grepl("CISCO",ETC$Subject_Fin),1,0)
ETC$CITRIXFLAG<-ifelse (grepl("CITRIX",ETC$Subject_Fin),1,0)
ETC$CLOCKFLAG<-ifelse (grepl("CLOCK",ETC$Subject_Fin),1,0)
ETC$CMBFLAG<-ifelse (grepl("CMB",ETC$Subject_Fin),1,0)
ETC$CODEFLAG<-ifelse (grepl("CODE",ETC$Subject_Fin),1,0)
ETC$COMINGFLAG<-ifelse (grepl("COMING",ETC$Subject_Fin),1,0)
ETC$COMMUNICATFLAG<-ifelse (grepl("COMMUNICAT",ETC$Subject_Fin),1,0)
ETC$COMMUNICATIONFLAG<-ifelse (grepl("COMMUNICATION",ETC$Subject_Fin),1,0)
ETC$COMMUNICATORFLAG<-ifelse (grepl("COMMUNICATOR",ETC$Subject_Fin),1,0)
ETC$COMPANYFLAG<-ifelse (grepl("COMPANY",ETC$Subject_Fin),1,0)
ETC$COMPUTERFLAG<-ifelse (grepl("COMPUTER",ETC$Subject_Fin),1,0)
ETC$COMPUTERSFLAG<-ifelse (grepl("COMPUTERS",ETC$Subject_Fin),1,0)
ETC$CONFERENCEFLAG<-ifelse (grepl("CONFERENCE",ETC$Subject_Fin),1,0)
ETC$CONFIGFLAG<-ifelse (grepl("CONFIG",ETC$Subject_Fin),1,0)
ETC$CONFIRMATIONFLAG<-ifelse (grepl("CONFIRMATION",ETC$Subject_Fin),1,0)
ETC$CONNECTFLAG<-ifelse (grepl("CONNECT",ETC$Subject_Fin),1,0)
ETC$CONNECTIONFLAG<-ifelse (grepl("CONNECTION",ETC$Subject_Fin),1,0)
ETC$CONNECTIVITYFLAG<-ifelse (grepl("CONNECTIVITY",ETC$Subject_Fin),1,0)
ETC$CONSULTANTSFLAG<-ifelse (grepl("CONSULTANTS",ETC$Subject_Fin),1,0)
ETC$CONTACTFLAG<-ifelse (grepl("CONTACT",ETC$Subject_Fin),1,0)
ETC$CONTRACTFLAG<-ifelse (grepl("CONTRACT",ETC$Subject_Fin),1,0)
ETC$CONTROLLERFLAG<-ifelse (grepl("CONTROLLER",ETC$Subject_Fin),1,0)
ETC$COPIERFLAG<-ifelse (grepl("COPIER",ETC$Subject_Fin),1,0)
ETC$COPYFLAG<-ifelse (grepl("COPY",ETC$Subject_Fin),1,0)
ETC$COPYMACHINEFLAG<-ifelse (grepl("COPY MACHINE",ETC$Subject_Fin),1,0)
ETC$CORDINGMACHINEFLAG<-ifelse (grepl("CORDING MACHINE",ETC$Subject_Fin),1,0)
ETC$CORRECTFLAG<-ifelse (grepl("CORRECT",ETC$Subject_Fin),1,0)
ETC$CPUFLAG<-ifelse (grepl("CPU",ETC$Subject_Fin),1,0)
ETC$CRASHFLAG<-ifelse (grepl("CRASH",ETC$Subject_Fin),1,0)
ETC$CREATFLAG<-ifelse (grepl("CREAT",ETC$Subject_Fin),1,0)
ETC$CREATEFLAG<-ifelse (grepl("CREATE",ETC$Subject_Fin),1,0)
ETC$CREATIONFLAG<-ifelse (grepl("CREATION",ETC$Subject_Fin),1,0)
ETC$CRITICALFLAG<-ifelse (grepl("CRITICAL",ETC$Subject_Fin),1,0)
ETC$CROSSFLAG<-ifelse (grepl("CROSS",ETC$Subject_Fin),1,0)
ETC$CSRSFLAG<-ifelse (grepl("CSRS",ETC$Subject_Fin),1,0)
ETC$CUSTOMERFLAG<-ifelse (grepl("CUSTOMER",ETC$Subject_Fin),1,0)
ETC$CUTFLAG<-ifelse (grepl("CUT",ETC$Subject_Fin),1,0)
ETC$DAMAGEFLAG<-ifelse (grepl("DAMAGE",ETC$Subject_Fin),1,0)
ETC$DAMAGFLAG<-ifelse (grepl("DAMAG",ETC$Subject_Fin),1,0)
ETC$DASHBOARDFLAG<-ifelse (grepl("DASHBOARD",ETC$Subject_Fin),1,0)
ETC$DATABASEFLAG<-ifelse (grepl("DATABASE",ETC$Subject_Fin),1,0)
ETC$DATAFLAG<-ifelse (grepl("DATA",ETC$Subject_Fin),1,0)
ETC$DATEFLAG<-ifelse (grepl("DATE",ETC$Subject_Fin),1,0)
ETC$DAYSFLAG<-ifelse (grepl("DAYS",ETC$Subject_Fin),1,0)
ETC$DEACTIVATIONFLAG<-ifelse (grepl("DEACTIVATION",ETC$Subject_Fin),1,0)
ETC$DEADFLAG<-ifelse (grepl("DEAD",ETC$Subject_Fin),1,0)
ETC$DEALERLOCATORFLAG<-ifelse (grepl("DEALER LOCATOR",ETC$Subject_Fin),1,0)
ETC$DEBUGFLAG<-ifelse (grepl("DEBUG",ETC$Subject_Fin),1,0)
ETC$DEFAULTFLAG<-ifelse (grepl("DEFAULT",ETC$Subject_Fin),1,0)
ETC$DELAYFLAG<-ifelse (grepl("DELAY",ETC$Subject_Fin),1,0)
ETC$DELETFLAG<-ifelse (grepl("DELET",ETC$Subject_Fin),1,0)
ETC$DELIVERFLAG<-ifelse (grepl("DELIVER",ETC$Subject_Fin),1,0)
ETC$DELLFLAG<-ifelse (grepl("DELL",ETC$Subject_Fin),1,0)
ETC$DEMANDFLAG<-ifelse (grepl("DEMAND",ETC$Subject_Fin),1,0)
ETC$DENIEDFLAG<-ifelse (grepl("DENIED",ETC$Subject_Fin),1,0)
ETC$DENTIRXFLAG<-ifelse (grepl("DENTIRX",ETC$Subject_Fin),1,0)
ETC$DENTRIXFLAG<-ifelse (grepl("DENTRIX",ETC$Subject_Fin),1,0)
ETC$DENVERFLAG<-ifelse (grepl("DENVER",ETC$Subject_Fin),1,0)
ETC$DESKFLAG<-ifelse (grepl("DESK",ETC$Subject_Fin),1,0)
ETC$DESKTOPFLAG<-ifelse (grepl("DESKTOP",ETC$Subject_Fin),1,0)
ETC$DETAILSFLAG<-ifelse (grepl("DETAILS",ETC$Subject_Fin),1,0)
ETC$DETECTFLAG<-ifelse (grepl("DETECT",ETC$Subject_Fin),1,0)
ETC$DEVFLAG<-ifelse (grepl("DEV",ETC$Subject_Fin),1,0)
ETC$DEVICEFLAG<-ifelse (grepl("DEVICE",ETC$Subject_Fin),1,0)
ETC$DHCPFLAG<-ifelse (grepl("DHCP",ETC$Subject_Fin),1,0)
ETC$DIAGNOSFLAG<-ifelse (grepl("DIAGNOS",ETC$Subject_Fin),1,0)
ETC$DIALFLAG<-ifelse (grepl("DIAL",ETC$Subject_Fin),1,0)
ETC$DIDFLAG<-ifelse (grepl("DID",ETC$Subject_Fin),1,0)
ETC$DIRECTFLAG<-ifelse (grepl("DIRECT",ETC$Subject_Fin),1,0)
ETC$DIRECTCONNECTFLAG<-ifelse (grepl("DIRECT CONNECT",ETC$Subject_Fin),1,0)
ETC$DIRECTWEBFLAG<-ifelse (grepl("DIRECT WEB",ETC$Subject_Fin),1,0)
ETC$DIRECTWEBFLAG2<-ifelse (grepl("DIRECTWEB",ETC$Subject_Fin),1,0)
ETC$DISABLEFLAG<-ifelse (grepl("DISABLE",ETC$Subject_Fin),1,0)
ETC$DISCHARGEFLAG<-ifelse (grepl("DISCHARGE",ETC$Subject_Fin),1,0)
ETC$DISKFLAG<-ifelse (grepl("DISK",ETC$Subject_Fin),1,0)
ETC$DOCFLAG<-ifelse (grepl("DOC",ETC$Subject_Fin),1,0)
ETC$DOCUMENTFLAG<-ifelse (grepl("DOCUMENT",ETC$Subject_Fin),1,0)
ETC$DOESNOTFLAG<-ifelse (grepl("DOES NOT",ETC$Subject_Fin),1,0)
ETC$DOESNOTWORKFLAG<-ifelse (grepl("DOES NOT WORK",ETC$Subject_Fin),1,0)
ETC$DOESNTFEEDFLAG<-ifelse (grepl("DOESNT FEED",ETC$Subject_Fin),1,0)
ETC$DOESNTFLAG<-ifelse (grepl("DOESNT",ETC$Subject_Fin),1,0)
ETC$DOESNTWORKFLAG<-ifelse (grepl("DOESNT WORK",ETC$Subject_Fin),1,0)
ETC$DOFLAG<-ifelse (grepl("DO",ETC$Subject_Fin),1,0)
ETC$DOMAINFLAG<-ifelse (grepl("DOMAIN",ETC$Subject_Fin),1,0)
ETC$DONGLEFLAG<-ifelse (grepl("DONGLE",ETC$Subject_Fin),1,0)
ETC$DOSAGEFLAG<-ifelse (grepl("DOSAGE",ETC$Subject_Fin),1,0)
ETC$DOWNFLAG<-ifelse (grepl("DOWN",ETC$Subject_Fin),1,0)
ETC$DOWNLOADFLAG<-ifelse (grepl("DOWNLOAD",ETC$Subject_Fin),1,0)
ETC$DOWNSTAIRFLAG<-ifelse (grepl("DOWNSTAIR",ETC$Subject_Fin),1,0)
ETC$DRIVEFLAG<-ifelse (grepl("DRIVE",ETC$Subject_Fin),1,0)
ETC$DRIVESFLAG<-ifelse (grepl("DRIVES",ETC$Subject_Fin),1,0)
ETC$DROPBOXFLAG<-ifelse (grepl("DROPBOX",ETC$Subject_Fin),1,0)
ETC$DROPFLAG<-ifelse (grepl("DROP",ETC$Subject_Fin),1,0)
ETC$ECCFLAG<-ifelse (grepl("ECC",ETC$Subject_Fin),1,0)
ETC$ECLIPSYSFLAG<-ifelse (grepl("ECLIPSYS",ETC$Subject_Fin),1,0)
ETC$ECLIPSYSFLAG2<-ifelse (grepl("ECLIPS",ETC$Subject_Fin),1,0)
ETC$ECLIPSYSFLAG3<-ifelse (grepl("ECLYPSIS",ETC$Subject_Fin),1,0)
ETC$EDITFLAG<-ifelse (grepl("EDIT",ETC$Subject_Fin),1,0)
ETC$EFTFLAG<-ifelse (grepl("EFT",ETC$Subject_Fin),1,0)
ETC$EKGFLAG<-ifelse (grepl("EKG",ETC$Subject_Fin),1,0)
ETC$ELINKFLAG<-ifelse (grepl("ELINK",ETC$Subject_Fin),1,0)
ETC$ELINKFLAG2<-ifelse (grepl("E LINK",ETC$Subject_Fin),1,0)
ETC$ELINKFLAG3<-ifelse (grepl("E-LINK",ETC$Subject_Fin),1,0)
ETC$EMAILFLAG<-ifelse (grepl("EMAIL",ETC$Subject_Fin),1,0)
ETC$EMAILFLAG2<-ifelse (grepl("E-MAIL",ETC$Subject_Fin),1,0)
ETC$EMAILFLAG3<-ifelse (grepl("E MAIL",ETC$Subject_Fin),1,0)
ETC$EMAILFLAG4<-ifelse (grepl("E -MAIL",ETC$Subject_Fin),1,0)
ETC$EMPLOYEEFLAG<-ifelse (grepl("EMPLOYEE",ETC$Subject_Fin),1,0)
ETC$ENERGYFLAG<-ifelse (grepl("ENERGY",ETC$Subject_Fin),1,0)
ETC$ENGINEERFLAG<-ifelse (grepl("ENGINEER",ETC$Subject_Fin),1,0)
ETC$ENTERFLAG<-ifelse (grepl("ENTER",ETC$Subject_Fin),1,0)
ETC$ENTERPRISEFLAG<-ifelse (grepl("ENTERPRISE",ETC$Subject_Fin),1,0)
ETC$ENTIREFLOORFLAG<-ifelse (grepl("ENTIRE FLOOR",ETC$Subject_Fin),1,0)
ETC$ENTIRETEAMFLAG<-ifelse (grepl("ENTIRE TEAM",ETC$Subject_Fin),1,0)
ETC$EP2FLAG<-ifelse (grepl("EP2",ETC$Subject_Fin),1,0)
ETC$EPDMFLAG<-ifelse (grepl("EPDM",ETC$Subject_Fin),1,0)
ETC$EPRESCRIBEFLAG<-ifelse (grepl("EPRESCRIBE",ETC$Subject_Fin),1,0)
ETC$EQ1FLAG<-ifelse (grepl("EQ1",ETC$Subject_Fin),1,0)
ETC$EQUIPMENTFLAG<-ifelse (grepl("EQUIPMENT",ETC$Subject_Fin),1,0)
ETC$ERRORFLAG<-ifelse (grepl("ERROR",ETC$Subject_Fin),1,0)
ETC$ESNAFLAG<-ifelse (grepl("ESNA",ETC$Subject_Fin),1,0)
ETC$ESUBMITFLAG<-ifelse (grepl("ESUBMIT",ETC$Subject_Fin),1,0)
ETC$EVENTFLAG<-ifelse (grepl("EVENT",ETC$Subject_Fin),1,0)
ETC$EVENTMANAGERFLAG<-ifelse (grepl("EVENT MANAGER",ETC$Subject_Fin),1,0)
ETC$EVERYBODYFLAG<-ifelse (grepl("EVERYBODY",ETC$Subject_Fin),1,0)
ETC$EVOFLAG<-ifelse (grepl("EVO",ETC$Subject_Fin),1,0)
ETC$EXCELFLAG<-ifelse (grepl("EXCEL",ETC$Subject_Fin),1,0)
ETC$EXCHANGEFLAG<-ifelse (grepl("EXCHANGE",ETC$Subject_Fin),1,0)
ETC$EXITCAREFLAG<-ifelse (grepl("EXIT CARE",ETC$Subject_Fin),1,0)
ETC$EXITCAREFLAG2<-ifelse (grepl("EXITCARE",ETC$Subject_Fin),1,0)
ETC$EXPENSFLAG<-ifelse (grepl("EXPENS",ETC$Subject_Fin),1,0)
ETC$EXPIREDFLAG<-ifelse (grepl("EXPIRED",ETC$Subject_Fin),1,0)
ETC$EXPIRFLAG<-ifelse (grepl("EXPIR",ETC$Subject_Fin),1,0)
ETC$EXPORTFLAG<-ifelse (grepl("EXPORT",ETC$Subject_Fin),1,0)
ETC$EXTENDFLAG<-ifelse (grepl("EXTEND",ETC$Subject_Fin),1,0)
ETC$EXTRANETFLAG<-ifelse (grepl("EXTRANET",ETC$Subject_Fin),1,0)
ETC$FABFLAG<-ifelse (grepl("FAB",ETC$Subject_Fin),1,0)
ETC$FABMAINT2FLAG<-ifelse (grepl("FABMAINT2",ETC$Subject_Fin),1,0)
ETC$FABRICFLAG<-ifelse (grepl("FABRIC",ETC$Subject_Fin),1,0)
ETC$FACESHEETFLAG<-ifelse (grepl("FACESHEET",ETC$Subject_Fin),1,0)
ETC$FACILITIESFLAG<-ifelse (grepl("FACILITIES",ETC$Subject_Fin),1,0)
ETC$FAILEDFLAG<-ifelse (grepl("FAILED",ETC$Subject_Fin),1,0)
ETC$FAILFLAG<-ifelse (grepl("FAIL",ETC$Subject_Fin),1,0)
ETC$FAILUREFLAG<-ifelse (grepl("FAILURE",ETC$Subject_Fin),1,0)
ETC$FAUXFLAG<-ifelse (grepl("FAUX",ETC$Subject_Fin),1,0)
ETC$FAXFLAG<-ifelse (grepl("FAX",ETC$Subject_Fin),1,0)
ETC$FEEDFLAG<-ifelse (grepl("FEED",ETC$Subject_Fin),1,0)
ETC$FEWFLAG<-ifelse (grepl("FEW",ETC$Subject_Fin),1,0)
ETC$FILEFLAG<-ifelse (grepl("FILE",ETC$Subject_Fin),1,0)
ETC$FILEZILLAFLAG<-ifelse (grepl("FILEZILLA",ETC$Subject_Fin),1,0)
ETC$FINDFLAG<-ifelse (grepl("FIND",ETC$Subject_Fin),1,0)
ETC$FINISHFLAG<-ifelse (grepl("FINISH",ETC$Subject_Fin),1,0)
ETC$FIREWALLFLAG<-ifelse (grepl("FIREWALL",ETC$Subject_Fin),1,0)
ETC$FIRMFLAG<-ifelse (grepl("FIRM",ETC$Subject_Fin),1,0)
ETC$FISFLAG<-ifelse (grepl("FIS",ETC$Subject_Fin),1,0)
ETC$FIXFLAG<-ifelse (grepl("FIX",ETC$Subject_Fin),1,0)
ETC$FLASHFLAG<-ifelse (grepl("FLASH",ETC$Subject_Fin),1,0)
ETC$FLOWSHEETFLAG<-ifelse (grepl("FLOWSHEET",ETC$Subject_Fin),1,0)
ETC$FLUCUATFLAG<-ifelse (grepl("FLUCUAT",ETC$Subject_Fin),1,0)
ETC$FOLDERFLAG<-ifelse (grepl("FOLDER",ETC$Subject_Fin),1,0)
ETC$FOLLOWFLAG<-ifelse (grepl("FOLLOW",ETC$Subject_Fin),1,0)
ETC$FORGOTFLAG<-ifelse (grepl("FORGOT",ETC$Subject_Fin),1,0)
ETC$FORMATFLAG<-ifelse (grepl("FORMAT",ETC$Subject_Fin),1,0)
ETC$FORWARDFLAG<-ifelse (grepl("FORWARD",ETC$Subject_Fin),1,0)
ETC$FOUNDFLAG<-ifelse (grepl("FOUND",ETC$Subject_Fin),1,0)
ETC$FRAMEFLAG<-ifelse (grepl("FRAME",ETC$Subject_Fin),1,0)
ETC$FREEFLAG<-ifelse (grepl("FREE",ETC$Subject_Fin),1,0)
ETC$FREEZEDFLAG<-ifelse (grepl("FREEZED",ETC$Subject_Fin),1,0)
ETC$FREEZESFLAG<-ifelse (grepl("FREEZES",ETC$Subject_Fin),1,0)
ETC$FREEZESSFLAG<-ifelse (grepl("FREEZESS",ETC$Subject_Fin),1,0)
ETC$FREEZFLAG<-ifelse (grepl("FREEZ",ETC$Subject_Fin),1,0)
ETC$FREEZINGFLAG<-ifelse (grepl("FREEZING",ETC$Subject_Fin),1,0)
ETC$FRONTIERFLAG<-ifelse (grepl("FRONTIER",ETC$Subject_Fin),1,0)
ETC$FROZEFLAG<-ifelse (grepl("FROZE",ETC$Subject_Fin),1,0)
ETC$FROZENFLAG<-ifelse (grepl("FROZEN",ETC$Subject_Fin),1,0)
ETC$FROZFLAG<-ifelse (grepl("FROZ",ETC$Subject_Fin),1,0)
ETC$FSFLAG<-ifelse (grepl("FS",ETC$Subject_Fin),1,0)
ETC$FTPFLAG<-ifelse (grepl("FTP",ETC$Subject_Fin),1,0)
ETC$FULLFLAG<-ifelse (grepl("FULL",ETC$Subject_Fin),1,0)
ETC$FUNCTIONFLAG<-ifelse (grepl("FUNCTION",ETC$Subject_Fin),1,0)
ETC$FUSERFLAG<-ifelse (grepl("FUSER",ETC$Subject_Fin),1,0)
ETC$FUSIONOPSFLAG<-ifelse (grepl("FUSIONOPS",ETC$Subject_Fin),1,0)
ETC$GAVSFLAG<-ifelse (grepl("GAVS",ETC$Subject_Fin),1,0)
ETC$GBFLAG<-ifelse (grepl("GB",ETC$Subject_Fin),1,0)
ETC$GEARFLAG<-ifelse (grepl("GEAR",ETC$Subject_Fin),1,0)
ETC$GENERALFLAG<-ifelse (grepl("GENERAL",ETC$Subject_Fin),1,0)
ETC$GERBERFLAG<-ifelse (grepl("GERBER",ETC$Subject_Fin),1,0)
ETC$GETFLAG<-ifelse (grepl("GET",ETC$Subject_Fin),1,0)
ETC$GETINFLAG<-ifelse (grepl("GET IN",ETC$Subject_Fin),1,0)
ETC$GIVFLAG<-ifelse (grepl("GIV",ETC$Subject_Fin),1,0)
ETC$GMAILFLAG<-ifelse (grepl("GMAIL",ETC$Subject_Fin),1,0)
ETC$GOFLAG<-ifelse (grepl("GO",ETC$Subject_Fin),1,0)
ETC$GOODSFLAG<-ifelse (grepl("GOODS",ETC$Subject_Fin),1,0)
ETC$GOOGLEFLAG<-ifelse (grepl("GOOGLE",ETC$Subject_Fin),1,0)
ETC$GRACEFLAG<-ifelse (grepl("GRACE",ETC$Subject_Fin),1,0)
ETC$GROUPFLAG<-ifelse (grepl("GROUP",ETC$Subject_Fin),1,0)
ETC$GROUPWISEFLAG<-ifelse (grepl("GROUPWISE",ETC$Subject_Fin),1,0)
ETC$HACKFLAG<-ifelse (grepl("HACK",ETC$Subject_Fin),1,0)
ETC$HALFFLAG<-ifelse (grepl("HALF",ETC$Subject_Fin),1,0)
ETC$HARDDRIVEFLAG<-ifelse (grepl("HARD DRIVE",ETC$Subject_Fin),1,0)
ETC$HARDFLAG<-ifelse (grepl("HARD",ETC$Subject_Fin),1,0)
ETC$HARDWAREFLAG<-ifelse (grepl("HARDWARE",ETC$Subject_Fin),1,0)
ETC$HDGUESTFLAG<-ifelse (grepl("HDGUEST",ETC$Subject_Fin),1,0)
ETC$HDNAFLAG<-ifelse (grepl("HDNA",ETC$Subject_Fin),1,0)
ETC$HEADSETFLAG<-ifelse (grepl("HEADSET",ETC$Subject_Fin),1,0)
ETC$HEADSETFLAG2<-ifelse (grepl("HEAD SET",ETC$Subject_Fin),1,0)
ETC$HEALTHFLAG<-ifelse (grepl("HEALTH",ETC$Subject_Fin),1,0)
ETC$HELPFLAG<-ifelse (grepl("HELP",ETC$Subject_Fin),1,0)
ETC$HELPDESKFLAG<-ifelse (grepl("HELP DESK",ETC$Subject_Fin),1,0)
ETC$HELPDESKFLAG2<-ifelse (grepl("HELPDESK",ETC$Subject_Fin),1,0)
ETC$HIDDENFLAG<-ifelse (grepl("HIDDEN",ETC$Subject_Fin),1,0)
ETC$HIGHFLAG<-ifelse (grepl("HIGH",ETC$Subject_Fin),1,0)
ETC$HINGINGFLAG<-ifelse (grepl("HINGING",ETC$Subject_Fin),1,0)
ETC$HIREFLAG<-ifelse (grepl("HIRE",ETC$Subject_Fin),1,0)
ETC$HOISTFLAG<-ifelse (grepl("HOIST",ETC$Subject_Fin),1,0)
ETC$HOISTSFLAG<-ifelse (grepl("HOISTS",ETC$Subject_Fin),1,0)
ETC$HORIZONFLAG<-ifelse (grepl("HORIZON",ETC$Subject_Fin),1,0)
ETC$HOSTFLAG<-ifelse (grepl("HOST",ETC$Subject_Fin),1,0)
ETC$HUNGFLAG<-ifelse (grepl("HUNG",ETC$Subject_Fin),1,0)
ETC$IDFLAG<-ifelse (grepl("ID",ETC$Subject_Fin),1,0)
ETC$IEFLAG<-ifelse (grepl("IE",ETC$Subject_Fin),1,0)
ETC$IMAGECASTFLAG<-ifelse (grepl("IMAGECAST",ETC$Subject_Fin),1,0)
ETC$IMAGECASTFLAG2<-ifelse (grepl("IMAGE CAST",ETC$Subject_Fin),1,0)
ETC$IMAGEFLAG<-ifelse (grepl("IMAGE",ETC$Subject_Fin),1,0)
ETC$IMPLEMENTFLAG<-ifelse (grepl("IMPLEMENT",ETC$Subject_Fin),1,0)
ETC$INCIDENTFLAG<-ifelse (grepl("INCIDENT",ETC$Subject_Fin),1,0)
ETC$INCORRECTFLAG<-ifelse (grepl("INCORRECT",ETC$Subject_Fin),1,0)
ETC$INDIVIDUALSFLAG<-ifelse (grepl("INDIVIDUALS",ETC$Subject_Fin),1,0)
ETC$INDUSOFTFLAG<-ifelse (grepl("INDUSOFT",ETC$Subject_Fin),1,0)
ETC$INFECTEDFLAG<-ifelse (grepl("INFECTED",ETC$Subject_Fin),1,0)
ETC$INFECTFLAG<-ifelse (grepl("INFECT",ETC$Subject_Fin),1,0)
ETC$INFOFLAG<-ifelse (grepl("INFO",ETC$Subject_Fin),1,0)
ETC$INFOREQUESTFLAG<-ifelse (grepl("INFOREQUEST",ETC$Subject_Fin),1,0)
ETC$ININFLAG<-ifelse (grepl("ININ",ETC$Subject_Fin),1,0)
ETC$INKFLAG<-ifelse (grepl("INK",ETC$Subject_Fin),1,0)
ETC$INSPECTIONFLAG<-ifelse (grepl("INSPECTION",ETC$Subject_Fin),1,0)
ETC$INSTALLFLAG<-ifelse (grepl("INSTALL",ETC$Subject_Fin),1,0)
ETC$INSTEADFLAG<-ifelse (grepl("INSTEAD",ETC$Subject_Fin),1,0)
ETC$INSURANCEFLAG<-ifelse (grepl("INSURANCE",ETC$Subject_Fin),1,0)
ETC$INTELLISILLFLAG<-ifelse (grepl("INTELLISILL",ETC$Subject_Fin),1,0)
ETC$INTERFACEFLAG<-ifelse (grepl("INTERFACE",ETC$Subject_Fin),1,0)
ETC$INTERMITTENTFLAG<-ifelse (grepl("INTERMITTENT",ETC$Subject_Fin),1,0)
ETC$INTERNETFLAG<-ifelse (grepl("INTERNET",ETC$Subject_Fin),1,0)
ETC$INTERNFLAG<-ifelse (grepl("INTERN",ETC$Subject_Fin),1,0)
ETC$INTERNALFLAG<-ifelse (grepl("INTERNAL",ETC$Subject_Fin),1,0)
ETC$INTERNATIONALFLAG<-ifelse (grepl("INTERNATIONAL",ETC$Subject_Fin),1,0)
ETC$INTERQUALFLAG<-ifelse (grepl("INTERQUAL",ETC$Subject_Fin),1,0)
ETC$INTRANETFLAG<-ifelse (grepl("INTRANET",ETC$Subject_Fin),1,0)
ETC$INVALIDFLAG<-ifelse (grepl("INVALID",ETC$Subject_Fin),1,0)
ETC$INVENTORYFLAG<-ifelse (grepl("INVENTORY",ETC$Subject_Fin),1,0)
ETC$INVERTFLAG<-ifelse (grepl("INVERT",ETC$Subject_Fin),1,0)
ETC$INVOICEFLAG<-ifelse (grepl("INVOICE",ETC$Subject_Fin),1,0)
ETC$INVOICESFLAG<-ifelse (grepl("INVOICES",ETC$Subject_Fin),1,0)
ETC$IPADFLAG<-ifelse (grepl("IPAD",ETC$Subject_Fin),1,0)
ETC$IPFLAG<-ifelse (grepl("IP",ETC$Subject_Fin),1,0)
ETC$IPHONEFLAG<-ifelse (grepl("IPHONE",ETC$Subject_Fin),1,0)
ETC$ISFLAG<-ifelse (grepl(" IS ",ETC$Subject_Fin),1,0)
ETC$ISNTFLAG<-ifelse (grepl("ISNT",ETC$Subject_Fin),1,0)
ETC$ISNTWORKFLAG<-ifelse (grepl("ISNT WORK",ETC$Subject_Fin),1,0)
ETC$ISSFLAG<-ifelse (grepl("ISS",ETC$Subject_Fin),1,0)
ETC$ISSUEDFLAG<-ifelse (grepl("ISSUED",ETC$Subject_Fin),1,0)
ETC$ISSUEFLAG<-ifelse (grepl("ISSUE",ETC$Subject_Fin),1,0)
ETC$ISSUESFLAG<-ifelse (grepl("ISSUES",ETC$Subject_Fin),1,0)
ETC$ITCHECKFLAG<-ifelse (grepl("ITCHECK",ETC$Subject_Fin),1,0)
ETC$ITEMFLAG<-ifelse (grepl("ITEM",ETC$Subject_Fin),1,0)
ETC$ITSUPPORTFLAG<-ifelse (grepl("ITSUPPORT",ETC$Subject_Fin),1,0)
ETC$ITSUPPORTFLAG2<-ifelse (grepl("IT SUPPORT",ETC$Subject_Fin),1,0)
ETC$ITUNESFLAG<-ifelse (grepl("ITUNES",ETC$Subject_Fin),1,0)
ETC$JAMFLAG<-ifelse (grepl("JAM",ETC$Subject_Fin),1,0)
ETC$JAVAFLAG<-ifelse (grepl("JAVA",ETC$Subject_Fin),1,0)
ETC$JINGFLAG<-ifelse (grepl("JING",ETC$Subject_Fin),1,0)
ETC$JIRAFLAG<-ifelse (grepl("JIRA",ETC$Subject_Fin),1,0)
ETC$JOBFLAG<-ifelse (grepl("JOB",ETC$Subject_Fin),1,0)
ETC$JOBSFLAG<-ifelse (grepl("JOBS",ETC$Subject_Fin),1,0)
ETC$JUNIPERFLAG<-ifelse (grepl("JUNIPER",ETC$Subject_Fin),1,0)
ETC$JUNKFLAG<-ifelse (grepl("JUNK",ETC$Subject_Fin),1,0)
ETC$KAIZENFLAG<-ifelse (grepl("KAIZEN",ETC$Subject_Fin),1,0)
ETC$KEYBOARDFLAG<-ifelse (grepl("KEYBOARD",ETC$Subject_Fin),1,0)
ETC$KICKEDOUTFLAG<-ifelse (grepl("KICKED OUT",ETC$Subject_Fin),1,0)
ETC$KIOSKFLAG<-ifelse (grepl("KIOSK",ETC$Subject_Fin),1,0)
ETC$KITACEFLAG<-ifelse (grepl("KIT ACE",ETC$Subject_Fin),1,0)
ETC$KNOWFLAG<-ifelse (grepl("KNOW",ETC$Subject_Fin),1,0)
ETC$KRONOSFLAG<-ifelse (grepl("KRONOS",ETC$Subject_Fin),1,0)
ETC$LABELFLAG<-ifelse (grepl("LABEL",ETC$Subject_Fin),1,0)
ETC$LABFLAG<-ifelse (grepl("LAB",ETC$Subject_Fin),1,0)
ETC$LABLABELFLAG<-ifelse (grepl("LAB LABEL",ETC$Subject_Fin),1,0)
ETC$LABLEFLAG<-ifelse (grepl("LABLE",ETC$Subject_Fin),1,0)
ETC$LABLESFLAG<-ifelse (grepl("LABLES",ETC$Subject_Fin),1,0)
ETC$LABREPORTFLAG<-ifelse (grepl("LAB REPORT",ETC$Subject_Fin),1,0)
ETC$LABTESTFLAG<-ifelse (grepl("LAB TEST",ETC$Subject_Fin),1,0)
ETC$LANFLAG<-ifelse (grepl("LAN",ETC$Subject_Fin),1,0)
ETC$LAPTOPFLAG<-ifelse (grepl("LAPTOP",ETC$Subject_Fin),1,0)
ETC$LASERFLAG<-ifelse (grepl("LASER",ETC$Subject_Fin),1,0)
ETC$LATENCYFLAG<-ifelse (grepl("LATENCY",ETC$Subject_Fin),1,0)
ETC$LATESTFLAG<-ifelse (grepl("LATEST",ETC$Subject_Fin),1,0)
ETC$LAUNCHFLAG<-ifelse (grepl("LAUNCH",ETC$Subject_Fin),1,0)
ETC$LAUNCHINGFLAG<-ifelse (grepl("LAUNCHING",ETC$Subject_Fin),1,0)
ETC$LESSTHANFLAG<-ifelse (grepl("LESS THAN",ETC$Subject_Fin),1,0)
ETC$LICENSFLAG<-ifelse (grepl("LICENS",ETC$Subject_Fin),1,0)
ETC$LIMITFLAG<-ifelse (grepl("LIMIT",ETC$Subject_Fin),1,0)
ETC$LINEFLAG<-ifelse (grepl("LINE",ETC$Subject_Fin),1,0)
ETC$LINESFLAG<-ifelse (grepl("LINES",ETC$Subject_Fin),1,0)
ETC$LINKFLAG<-ifelse (grepl("LINK",ETC$Subject_Fin),1,0)
ETC$LINKAGFLAG<-ifelse (grepl("LINKAG",ETC$Subject_Fin),1,0)
ETC$LINUXFLAG<-ifelse (grepl("LINUX",ETC$Subject_Fin),1,0)
ETC$LISTENFLAG<-ifelse (grepl("LISTEN",ETC$Subject_Fin),1,0)
ETC$LISTFLAG<-ifelse (grepl("LIST",ETC$Subject_Fin),1,0)
ETC$LITEFLAG<-ifelse (grepl("LITE",ETC$Subject_Fin),1,0)
ETC$LOADINGFLAG<-ifelse (grepl("LOADING",ETC$Subject_Fin),1,0)
ETC$LOANFLAG<-ifelse (grepl("LOAN",ETC$Subject_Fin),1,0)
ETC$LOCKFLAG<-ifelse (grepl("LOCK",ETC$Subject_Fin),1,0)
ETC$LOGFLAG<-ifelse (grepl("LOG",ETC$Subject_Fin),1,0)
ETC$LOGGINGFLAG<-ifelse (grepl("LOGGING",ETC$Subject_Fin),1,0)
ETC$LOGINFLAG<-ifelse (grepl("LOGIN",ETC$Subject_Fin),1,0)
ETC$LOGINFLAG2<-ifelse (grepl("LOG IN",ETC$Subject_Fin),1,0)
ETC$LOGONFLAG<-ifelse (grepl("LOGON",ETC$Subject_Fin),1,0)
ETC$LOGONFLAG2<-ifelse (grepl("LOG ON",ETC$Subject_Fin),1,0)
ETC$LOGOUTFLAG<-ifelse (grepl("LOGOUT",ETC$Subject_Fin),1,0)
ETC$LOGSOFFFLAG<-ifelse (grepl("LOGS OFF",ETC$Subject_Fin),1,0)
ETC$LONGFLAG<-ifelse (grepl("LONG",ETC$Subject_Fin),1,0)
ETC$LOOKFLAG<-ifelse (grepl("LOOK",ETC$Subject_Fin),1,0)
ETC$LOOPFLAG<-ifelse (grepl("LOOP",ETC$Subject_Fin),1,0)
ETC$LOOSFLAG<-ifelse (grepl("LOOS",ETC$Subject_Fin),1,0)
ETC$LOSFLAG<-ifelse (grepl("LOS",ETC$Subject_Fin),1,0)
ETC$LOSSFLAG<-ifelse (grepl("LOSS",ETC$Subject_Fin),1,0)
ETC$LOSTFLAG<-ifelse (grepl("LOST",ETC$Subject_Fin),1,0)
ETC$LOWQUALITYFLAG<-ifelse (grepl("LOW QUALITY",ETC$Subject_Fin),1,0)
ETC$LYNCFLAG<-ifelse (grepl("LYNC",ETC$Subject_Fin),1,0)
ETC$MACHINEFLAG<-ifelse (grepl("MACHINE",ETC$Subject_Fin),1,0)
ETC$MAILFLAG<-ifelse (grepl("MAIL",ETC$Subject_Fin),1,0)
ETC$MAINTENANCEFLAG<-ifelse (grepl("MAINTENANCE",ETC$Subject_Fin),1,0)
ETC$MAKEFLAG<-ifelse (grepl("MAKE",ETC$Subject_Fin),1,0)
ETC$MALFUNCTIONFLAG<-ifelse (grepl("MALFUNCTION",ETC$Subject_Fin),1,0)
ETC$MALWAREFLAG<-ifelse (grepl("MALWARE",ETC$Subject_Fin),1,0)
ETC$MANAGERFLAG<-ifelse (grepl("MANAGER",ETC$Subject_Fin),1,0)
ETC$MANYUSERFLAG<-ifelse (grepl("MANY USER",ETC$Subject_Fin),1,0)
ETC$MAPFLAG<-ifelse (grepl("MAP",ETC$Subject_Fin),1,0)
ETC$MARKETFLAG<-ifelse (grepl("MARKET",ETC$Subject_Fin),1,0)
ETC$MASTERTICKETFLAG<-ifelse (grepl("MASTER TICKET",ETC$Subject_Fin),1,0)
ETC$MATFLAG<-ifelse (grepl("MAT",ETC$Subject_Fin),1,0)
ETC$MATERIALFLAG<-ifelse (grepl("MATERIAL",ETC$Subject_Fin),1,0)
ETC$MATERIALSFLAG<-ifelse (grepl("MATERIALS",ETC$Subject_Fin),1,0)
ETC$MCAFEEFLAG<-ifelse (grepl("MCAFEE",ETC$Subject_Fin),1,0)
ETC$MEDICATIONFLAG<-ifelse (grepl("MEDICATION",ETC$Subject_Fin),1,0)
ETC$MEETFLAG<-ifelse (grepl("MEET",ETC$Subject_Fin),1,0)
ETC$MEMBERFLAG<-ifelse (grepl("MEMBER",ETC$Subject_Fin),1,0)
ETC$MEMORYFLAG<-ifelse (grepl("MEMORY",ETC$Subject_Fin),1,0)
ETC$MENUFLAG<-ifelse (grepl("MENU",ETC$Subject_Fin),1,0)
ETC$MESSAGEFLAG<-ifelse (grepl("MESSAGE",ETC$Subject_Fin),1,0)
ETC$MICROSOFTFLAG<-ifelse (grepl("MICROSOFT",ETC$Subject_Fin),1,0)
ETC$MIGRATFLAG<-ifelse (grepl("MIGRAT",ETC$Subject_Fin),1,0)
ETC$MISALIGNEDFLAG<-ifelse (grepl("MISALIGNED",ETC$Subject_Fin),1,0)
ETC$MISCSHIPFLAG<-ifelse (grepl("MISCSHIP",ETC$Subject_Fin),1,0)
ETC$MISPRINTFLAG<-ifelse (grepl("MISPRINT",ETC$Subject_Fin),1,0)
ETC$MISSFLAG<-ifelse (grepl("MISS",ETC$Subject_Fin),1,0)
ETC$MISSINGFLAG<-ifelse (grepl("MISSING",ETC$Subject_Fin),1,0)
ETC$MODIFYFLAG<-ifelse (grepl("MODIFY",ETC$Subject_Fin),1,0)
ETC$MONITORFLAG<-ifelse (grepl("MONITOR",ETC$Subject_Fin),1,0)
ETC$MONITORINGFLAG<-ifelse (grepl("MONITORING",ETC$Subject_Fin),1,0)
ETC$MOUSEFLAG<-ifelse (grepl("MOUSE",ETC$Subject_Fin),1,0)
ETC$MOVEFLAG<-ifelse (grepl("MOVE",ETC$Subject_Fin),1,0)
ETC$MPLSFLAG<-ifelse (grepl("MPLS",ETC$Subject_Fin),1,0)
ETC$MSOFFICEFLAG<-ifelse (grepl("MS OFFICE",ETC$Subject_Fin),1,0)
ETC$MUSEFLAG<-ifelse (grepl("MUSE",ETC$Subject_Fin),1,0)
ETC$MYCOMPUTERFLAG<-ifelse (grepl("MY COMPUTER",ETC$Subject_Fin),1,0)
ETC$NEEDFLAG<-ifelse (grepl("NEED",ETC$Subject_Fin),1,0)
ETC$NETFLAG<-ifelse (grepl("NET",ETC$Subject_Fin),1,0)
ETC$NETWORKFLAG<-ifelse (grepl("NETWORK",ETC$Subject_Fin),1,0)
ETC$NEWFLAG<-ifelse (grepl("NEW",ETC$Subject_Fin),1,0)
ETC$NODEFLAG<-ifelse (grepl("NODE",ETC$Subject_Fin),1,0)
ETC$NOFLAG<-ifelse (grepl("NO ",ETC$Subject_Fin),1,0)
ETC$NOINFOFLAG<-ifelse (grepl("NO INFO",ETC$Subject_Fin),1,0)
ETC$NOINTERNETFLAG<-ifelse (grepl("NO INTERNET",ETC$Subject_Fin),1,0)
ETC$NONEOFTHEUSERFLAG<-ifelse (grepl("NONE OF THE USER",ETC$Subject_Fin),1,0)
ETC$NONFLAG<-ifelse (grepl("NON",ETC$Subject_Fin),1,0)
ETC$NONOFTHEUSERFLAG<-ifelse (grepl("NON OF THE USER",ETC$Subject_Fin),1,0)
ETC$NOSHOWFLAG<-ifelse (grepl("NO SHOW",ETC$Subject_Fin),1,0)
ETC$NOTABLEFLAG<-ifelse (grepl("NOT ABLE",ETC$Subject_Fin),1,0)
ETC$NOTACCEPTFLAG<-ifelse (grepl("NOT ACCEPT",ETC$Subject_Fin),1,0)
ETC$NOTACCESSFLAG<-ifelse (grepl("NOT ACCESS",ETC$Subject_Fin),1,0)
ETC$NOTALLOWFLAG<-ifelse (grepl("NOT ALLOW",ETC$Subject_Fin),1,0)
ETC$NOTAUTHENTICATEFLAG<-ifelse (grepl("NOT AUTHENTICATE",ETC$Subject_Fin),1,0)
ETC$NOTAVAILABLEFLAG<-ifelse (grepl("NOT AVAILABLE",ETC$Subject_Fin),1,0)
ETC$NOTBACKFLAG<-ifelse (grepl("NOT BACK",ETC$Subject_Fin),1,0)
ETC$NOTBEENCOMINGFLAG<-ifelse (grepl("NOT BEEN COMING",ETC$Subject_Fin),1,0)
ETC$NOTCHARGFLAG<-ifelse (grepl("NOT CHARG",ETC$Subject_Fin),1,0)
ETC$NOTCOMINGUPFLAG<-ifelse (grepl("NOT COMING UP",ETC$Subject_Fin),1,0)
ETC$NOTCOMMUNICATINGFLAG<-ifelse (grepl("NOT COMMUNICATING",ETC$Subject_Fin),1,0)
ETC$NOTCONNECTFLAG<-ifelse (grepl("NOT CONNECT",ETC$Subject_Fin),1,0)
ETC$NOTCUTFLAG<-ifelse (grepl("NOT CUT",ETC$Subject_Fin),1,0)
ETC$NOTESFLAG<-ifelse (grepl("NOTES",ETC$Subject_Fin),1,0)
ETC$NOTEXCEPTFLAG<-ifelse (grepl("NOT EXCEPT",ETC$Subject_Fin),1,0)
ETC$NOTFLAG<-ifelse (grepl("NOT ",ETC$Subject_Fin),1,0)
ETC$NOTFOUNDFLAG<-ifelse (grepl("NOT FOUND",ETC$Subject_Fin),1,0)
ETC$NOTFUNCTIONFLAG<-ifelse (grepl("NOT FUNCTION",ETC$Subject_Fin),1,0)
ETC$NOTGETFLAG<-ifelse (grepl("NOT GET",ETC$Subject_Fin),1,0)
ETC$NOTGIVFLAG<-ifelse (grepl("NOT GIV",ETC$Subject_Fin),1,0)
ETC$NOTGOINGFLAG<-ifelse (grepl("NOT GOING",ETC$Subject_Fin),1,0)
ETC$NOTICEFLAG<-ifelse (grepl("NOTICE",ETC$Subject_Fin),1,0)
ETC$NOTIFFLAG<-ifelse (grepl("NOTIF",ETC$Subject_Fin),1,0)
ETC$NOTIFICATIONFLAG<-ifelse (grepl("NOTIFICATION",ETC$Subject_Fin),1,0)
ETC$NOTKEEPFLAG<-ifelse (grepl("NOT KEEP",ETC$Subject_Fin),1,0)
ETC$NOTPICKUPFLAG<-ifelse (grepl("NOT PICK UP",ETC$Subject_Fin),1,0)
ETC$NOTPRINTFLAG<-ifelse (grepl("NOT PRINT",ETC$Subject_Fin),1,0)
ETC$NOTRECEIVFLAG<-ifelse (grepl("NOT RECEIV",ETC$Subject_Fin),1,0)
ETC$NOTRESPONDFLAG<-ifelse (grepl("NOT RESPOND",ETC$Subject_Fin),1,0)
ETC$NOTRESPONDINGFLAG<-ifelse (grepl("NOT RESPONDING",ETC$Subject_Fin),1,0)
ETC$NOTSCANFLAG<-ifelse (grepl("NOT SCAN",ETC$Subject_Fin),1,0)
ETC$NOTSHOWFLAG<-ifelse (grepl("NOT SHOW",ETC$Subject_Fin),1,0)
ETC$NOTSTARTFLAG<-ifelse (grepl("NOT START",ETC$Subject_Fin),1,0)
ETC$NOTTRANSFERFLAG<-ifelse (grepl("NOT TRANSFER",ETC$Subject_Fin),1,0)
ETC$NOTUPDATFLAG<-ifelse (grepl("NOT UPDAT",ETC$Subject_Fin),1,0)
ETC$NOTWORKFLAG<-ifelse (grepl("NOT WORK",ETC$Subject_Fin),1,0)
ETC$NOTWORKINGFLAG<-ifelse (grepl("NOT WORKING",ETC$Subject_Fin),1,0)
ETC$NOVELLFLAG<-ifelse (grepl("NOVELL",ETC$Subject_Fin),1,0)
ETC$OFFCENTERFLAG<-ifelse (grepl("OFF CENTER",ETC$Subject_Fin),1,0)
ETC$OFFFLAG<-ifelse (grepl("OFF",ETC$Subject_Fin),1,0)
ETC$OFFICEFLAG<-ifelse (grepl("OFFICE",ETC$Subject_Fin),1,0)
ETC$OFFLINEFLAG<-ifelse (grepl("OFFLINE",ETC$Subject_Fin),1,0)
ETC$OFFLINEFLAG2<-ifelse (grepl("OFF LINE",ETC$Subject_Fin),1,0)
ETC$ONLINEFLAG<-ifelse (grepl("ONLINE",ETC$Subject_Fin),1,0)
ETC$OPENFLAG<-ifelse (grepl("OPEN",ETC$Subject_Fin),1,0)
ETC$OPERATINGSYSTEMFLAG<-ifelse (grepl("OPERATING SYSTEM",ETC$Subject_Fin),1,0)
ETC$ORACLEFLAG<-ifelse (grepl("ORACLE",ETC$Subject_Fin),1,0)
ETC$ORDERFLAG<-ifelse (grepl("ORDER",ETC$Subject_Fin),1,0)
ETC$ORDERSFLAG<-ifelse (grepl("ORDERS",ETC$Subject_Fin),1,0)
ETC$OSFLAG<-ifelse (grepl("OS",ETC$Subject_Fin),1,0)
ETC$OUTAGEFLAG<-ifelse (grepl("OUTAGE",ETC$Subject_Fin),1,0)
ETC$OUTDATEDFLAG<-ifelse (grepl("OUTDATED",ETC$Subject_Fin),1,0)
ETC$OUTFLAG<-ifelse (grepl("OUT",ETC$Subject_Fin),1,0)
ETC$OUTLOOKFLAG<-ifelse (grepl("OUTLOOK",ETC$Subject_Fin),1,0)
ETC$OUTPUTFLAG<-ifelse (grepl("OUTPUT",ETC$Subject_Fin),1,0)
ETC$OUTSIDEFLAG<-ifelse (grepl("OUTSIDE",ETC$Subject_Fin),1,0)
ETC$OVERFLAG<-ifelse (grepl("OVER",ETC$Subject_Fin),1,0)
ETC$OWNFLAG<-ifelse (grepl("OWN",ETC$Subject_Fin),1,0)
ETC$OZFLAG<-ifelse (grepl("OZ",ETC$Subject_Fin),1,0)
ETC$PACKETFLAG<-ifelse (grepl("PACKET",ETC$Subject_Fin),1,0)
ETC$PACKFLAG<-ifelse (grepl("PACK",ETC$Subject_Fin),1,0)
ETC$PACKINGLISTFLAG<-ifelse (grepl("PACKING LIST",ETC$Subject_Fin),1,0)
ETC$PACKINGSLIPFLAG<-ifelse (grepl("PACKING SLIP",ETC$Subject_Fin),1,0)
ETC$PACKLISTFLAG<-ifelse (grepl("PACK LIST",ETC$Subject_Fin),1,0)
ETC$PAFLAG<-ifelse (grepl("PA",ETC$Subject_Fin),1,0)
ETC$PAGEFLAG<-ifelse (grepl("PAGE",ETC$Subject_Fin),1,0)
ETC$PARTFLAG<-ifelse (grepl("PART",ETC$Subject_Fin),1,0)
ETC$PARTSFLAG<-ifelse (grepl("PARTS",ETC$Subject_Fin),1,0)
ETC$PASSWORDFLAG<-ifelse (grepl("PASSWORD",ETC$Subject_Fin),1,0)
ETC$PATCHFLAG<-ifelse (grepl("PATCH",ETC$Subject_Fin),1,0)
ETC$PATCOMFLAG<-ifelse (grepl("PATCOM",ETC$Subject_Fin),1,0)
ETC$PATEINTFLAG<-ifelse (grepl("PATEINT",ETC$Subject_Fin),1,0)
ETC$PATHFLAG<-ifelse (grepl("PATH",ETC$Subject_Fin),1,0)
ETC$PATIENTFLAG<-ifelse (grepl("PATIENT",ETC$Subject_Fin),1,0)
ETC$PAYFLAG<-ifelse (grepl("PAY",ETC$Subject_Fin),1,0)
ETC$PAYROLLFLAG<-ifelse (grepl("PAYROLL",ETC$Subject_Fin),1,0)
ETC$PCFLAG<-ifelse (grepl("PC",ETC$Subject_Fin),1,0)
ETC$PCARDFLAG<-ifelse (grepl("PCARD",ETC$Subject_Fin),1,0)
ETC$PCSFLAG<-ifelse (grepl("PCS",ETC$Subject_Fin),1,0)
ETC$PDFFLAG<-ifelse (grepl("PDF",ETC$Subject_Fin),1,0)
ETC$PEDOMETERFLAG<-ifelse (grepl("PEDOMETER",ETC$Subject_Fin),1,0)
ETC$PERFORMANCEFLAG<-ifelse (grepl("PERFORMANCE",ETC$Subject_Fin),1,0)
ETC$PERFORMINGFLAG<-ifelse (grepl("PERFORMING",ETC$Subject_Fin),1,0)
ETC$PERMISSIONFLAG<-ifelse (grepl("PERMISSION",ETC$Subject_Fin),1,0)
ETC$PHONEFLAG<-ifelse (grepl("PHONE",ETC$Subject_Fin),1,0)
ETC$PHONEQUEUEFLAG<-ifelse (grepl("PHONE QUEUE",ETC$Subject_Fin),1,0)
ETC$PHONESFLAG<-ifelse (grepl("PHONES",ETC$Subject_Fin),1,0)
ETC$PICKFLAG<-ifelse (grepl("PICK",ETC$Subject_Fin),1,0)
ETC$PLACEFLAG<-ifelse (grepl("PLACE",ETC$Subject_Fin),1,0)
ETC$PLATEFLAG<-ifelse (grepl("PLATE",ETC$Subject_Fin),1,0)
ETC$PLYMOUTHFLAG<-ifelse (grepl("PLYMOUTH",ETC$Subject_Fin),1,0)
ETC$PLMFLAG<-ifelse (grepl("PLM",ETC$Subject_Fin),1,0)
ETC$POFLAG<-ifelse (grepl("PO",ETC$Subject_Fin),1,0)
ETC$POORFLAG<-ifelse (grepl("POOR",ETC$Subject_Fin),1,0)
ETC$POORQUALITYFLAG<-ifelse (grepl("POOR QUALITY",ETC$Subject_Fin),1,0)
ETC$PORTFLAG<-ifelse (grepl("PORT",ETC$Subject_Fin),1,0)
ETC$PORTABLEFLAG<-ifelse (grepl("PORTABLE",ETC$Subject_Fin),1,0)
ETC$PORTALFLAG<-ifelse (grepl("PORTAL",ETC$Subject_Fin),1,0)
ETC$PORTSFLAG<-ifelse (grepl("PORTS",ETC$Subject_Fin),1,0)
ETC$POSITIONFLAG<-ifelse (grepl("POSITION",ETC$Subject_Fin),1,0)
ETC$POWERFLAG<-ifelse (grepl("POWER",ETC$Subject_Fin),1,0)
ETC$POWERINGONFLAG<-ifelse (grepl("POWERING ON",ETC$Subject_Fin),1,0)
ETC$PRESCRIBEFLAG<-ifelse (grepl("PRESCRIBE",ETC$Subject_Fin),1,0)
ETC$PRESCRIPTIONFLAG<-ifelse (grepl("PRESCRIPTION",ETC$Subject_Fin),1,0)
ETC$PREVILEGEFLAG<-ifelse (grepl("PREVILEGE",ETC$Subject_Fin),1,0)
ETC$PRIMARYFLAG<-ifelse (grepl("PRIMARY",ETC$Subject_Fin),1,0)
ETC$PRINTEDFLAG<-ifelse (grepl("PRINTED",ETC$Subject_Fin),1,0)
ETC$PRINTERFLAG<-ifelse (grepl("PRINTER",ETC$Subject_Fin),1,0)
ETC$PRINTFLAG<-ifelse (grepl("PRINT",ETC$Subject_Fin),1,0)
ETC$PRINTINGFLAG<-ifelse (grepl("PRINTING",ETC$Subject_Fin),1,0)
ETC$PRIORITYFLAG<-ifelse (grepl("PRIORITY",ETC$Subject_Fin),1,0)
ETC$PRISCRIPTIONSFLAG<-ifelse (grepl("PRISCRIPTIONS",ETC$Subject_Fin),1,0)
ETC$PRIVILEDGESFLAG<-ifelse (grepl("PRIVILEDGES",ETC$Subject_Fin),1,0)
ETC$PRIVILEGEFLAG<-ifelse (grepl("PRIVILEGE",ETC$Subject_Fin),1,0)
ETC$PRIVILFLAG<-ifelse (grepl("PRIVIL",ETC$Subject_Fin),1,0)
ETC$PROBLEMFLAG<-ifelse (grepl("PROBLEM",ETC$Subject_Fin),1,0)
ETC$PROBLEMSFLAG<-ifelse (grepl("PROBLEMS",ETC$Subject_Fin),1,0)
ETC$PROCESSFLAG<-ifelse (grepl("PROCESS",ETC$Subject_Fin),1,0)
ETC$PRODUCTFLAG<-ifelse (grepl("PRODUCT",ETC$Subject_Fin),1,0)
ETC$PRODUCTIONFLAG<-ifelse (grepl("PRODUCTION",ETC$Subject_Fin),1,0)
ETC$PROGRAMFLAG<-ifelse (grepl("PROGRAM",ETC$Subject_Fin),1,0)
ETC$PROJECTFLAG<-ifelse (grepl("PROJECT",ETC$Subject_Fin),1,0)
ETC$PROVATIONFLAG<-ifelse (grepl("PROVATION",ETC$Subject_Fin),1,0)
ETC$PTOFLAG<-ifelse (grepl("PTO",ETC$Subject_Fin),1,0)
ETC$PTOOLFLAG<-ifelse (grepl("PTOOL",ETC$Subject_Fin),1,0)
ETC$PTSFLAG<-ifelse (grepl("PTS",ETC$Subject_Fin),1,0)
ETC$PULLFLAG<-ifelse (grepl("PULL",ETC$Subject_Fin),1,0)
ETC$PULLUPFLAG<-ifelse (grepl("PULL UP",ETC$Subject_Fin),1,0)
ETC$PUNCHFLAG<-ifelse (grepl("PUNCH",ETC$Subject_Fin),1,0)
ETC$PURCHFLAG<-ifelse (grepl("PURCH",ETC$Subject_Fin),1,0)
ETC$PWFLAG<-ifelse (grepl("PW",ETC$Subject_Fin),1,0)
ETC$QUEUEFLAG<-ifelse (grepl("QUEUE",ETC$Subject_Fin),1,0)
ETC$QUICKFLAG<-ifelse (grepl("QUICK",ETC$Subject_Fin),1,0)
ETC$QUITWORKFLAG<-ifelse (grepl("QUIT WORK",ETC$Subject_Fin),1,0)
ETC$QUOTEFLAG<-ifelse (grepl("QUOTE",ETC$Subject_Fin),1,0)
ETC$RAILFLAG<-ifelse (grepl("RAIL",ETC$Subject_Fin),1,0)
ETC$RAILSAWFLAG<-ifelse (grepl("RAIL SAW",ETC$Subject_Fin),1,0)
ETC$RAMFLAG<-ifelse (grepl("RAM",ETC$Subject_Fin),1,0)
ETC$REACTIVATEFLAG<-ifelse (grepl("REACTIVATE",ETC$Subject_Fin),1,0)
ETC$READSOFTFLAG<-ifelse (grepl("READSOFT",ETC$Subject_Fin),1,0)
ETC$REBOOTFLAG<-ifelse (grepl("REBOOT",ETC$Subject_Fin),1,0)
ETC$RECEIVEFLAG<-ifelse (grepl("RECEIVE",ETC$Subject_Fin),1,0)
ETC$RECEIVERFLAG<-ifelse (grepl("RECEIVER",ETC$Subject_Fin),1,0)
ETC$RECEIVFLAG<-ifelse (grepl("RECEIV",ETC$Subject_Fin),1,0)
ETC$RECORDFLAG<-ifelse (grepl("RECORD",ETC$Subject_Fin),1,0)
ETC$RECRUITFLAG<-ifelse (grepl("RECRUIT",ETC$Subject_Fin),1,0)
ETC$REDUNDANCYFLAG<-ifelse (grepl("REDUNDANCY",ETC$Subject_Fin),1,0)
ETC$REDUNDANTFLAG<-ifelse (grepl("REDUNDANT",ETC$Subject_Fin),1,0)
ETC$REDUNDANTLINKFLAG<-ifelse (grepl("REDUNDANT LINK",ETC$Subject_Fin),1,0)
ETC$REFUFLAG<-ifelse (grepl("REFU",ETC$Subject_Fin),1,0)
ETC$REFUSOLFLAG<-ifelse (grepl("REFUSOL",ETC$Subject_Fin),1,0)
ETC$REGFLAG<-ifelse (grepl("REG ",ETC$Subject_Fin),1,0)
ETC$REGISTERFLAG<-ifelse (grepl("REGISTER",ETC$Subject_Fin),1,0)
ETC$REGISTRATIONFLAG<-ifelse (grepl("REGISTRATION",ETC$Subject_Fin),1,0)
ETC$REGLABELFLAG<-ifelse (grepl("REG LABEL",ETC$Subject_Fin),1,0)
ETC$RELEASEFLAG<-ifelse (grepl("RELEASE",ETC$Subject_Fin),1,0)
ETC$REMINDFLAG<-ifelse (grepl("REMIND",ETC$Subject_Fin),1,0)
ETC$REMOTEDESKTOPFLAG<-ifelse (grepl("REMOTE DESKTOP",ETC$Subject_Fin),1,0)
ETC$REMOTEFLAG<-ifelse (grepl("REMOTE",ETC$Subject_Fin),1,0)
ETC$REMOVFLAG<-ifelse (grepl("REMOV",ETC$Subject_Fin),1,0)
ETC$REMOVEFLAG<-ifelse (grepl("REMOVE",ETC$Subject_Fin),1,0)
ETC$RENEWFLAG<-ifelse (grepl("RENEW",ETC$Subject_Fin),1,0)
ETC$REPAIRFLAG<-ifelse (grepl("REPAIR",ETC$Subject_Fin),1,0)
ETC$REPLACEFLAG<-ifelse (grepl("REPLACE",ETC$Subject_Fin),1,0)
ETC$REPORTFLAG<-ifelse (grepl("REPORT",ETC$Subject_Fin),1,0)
ETC$REPORTINGFLAG<-ifelse (grepl("REPORTING",ETC$Subject_Fin),1,0)
ETC$REPUBLICFLAG<-ifelse (grepl("REPUBLIC",ETC$Subject_Fin),1,0)
ETC$REQFLAG<-ifelse (grepl("REQ",ETC$Subject_Fin),1,0)
ETC$REQUESTFLAG<-ifelse (grepl("REQUEST",ETC$Subject_Fin),1,0)
ETC$REQUIREDFLAG<-ifelse (grepl("REQUIRED",ETC$Subject_Fin),1,0)
ETC$RESETFLAG<-ifelse (grepl("RESET",ETC$Subject_Fin),1,0)
ETC$RESETFLAG2<-ifelse (grepl("RE-SET",ETC$Subject_Fin),1,0)
ETC$RESOURCEFLAG<-ifelse (grepl("RESOURCE",ETC$Subject_Fin),1,0)
ETC$RESPONDFLAG<-ifelse (grepl("RESPOND",ETC$Subject_Fin),1,0)
ETC$RESTARTEDFLAG<-ifelse (grepl("RESTARTED",ETC$Subject_Fin),1,0)
ETC$RESTARTFLAG<-ifelse (grepl("RESTART",ETC$Subject_Fin),1,0)
ETC$RESTARTINGFLAG<-ifelse (grepl("RESTARTING",ETC$Subject_Fin),1,0)
ETC$RESTORFLAG<-ifelse (grepl("RESTOR",ETC$Subject_Fin),1,0)
ETC$RESULTSFLAG<-ifelse (grepl("RESULTS",ETC$Subject_Fin),1,0)
ETC$RIBBONFLAG<-ifelse (grepl("RIBBON",ETC$Subject_Fin),1,0)
ETC$RIGHTFLAG<-ifelse (grepl("RIGHT",ETC$Subject_Fin),1,0)
ETC$RIGHTSFLAG<-ifelse (grepl("RIGHTS",ETC$Subject_Fin),1,0)
ETC$ROLEFLAG<-ifelse (grepl("ROLE",ETC$Subject_Fin),1,0)
ETC$ROLLERFLAG<-ifelse (grepl("ROLLER",ETC$Subject_Fin),1,0)
ETC$ROLLFLAG<-ifelse (grepl("ROLL",ETC$Subject_Fin),1,0)
ETC$ROUTEFLAG<-ifelse (grepl("ROUTE",ETC$Subject_Fin),1,0)
ETC$ROUTERFLAG<-ifelse (grepl("ROUTER",ETC$Subject_Fin),1,0)
ETC$RUBYFLAG<-ifelse (grepl("RUBY",ETC$Subject_Fin),1,0)
ETC$RUNFLAG<-ifelse (grepl("RUN",ETC$Subject_Fin),1,0)
ETC$RUNTIMEFLAG<-ifelse (grepl("RUN TIME",ETC$Subject_Fin),1,0)
ETC$SALEFLAG<-ifelse (grepl("SALE",ETC$Subject_Fin),1,0)
ETC$SALESFORCEFLAG<-ifelse (grepl("SALESFORCE",ETC$Subject_Fin),1,0)
ETC$SAMBAFLAG<-ifelse (grepl("SAMBA",ETC$Subject_Fin),1,0)
ETC$SAPFLAG<-ifelse (grepl("SAP",ETC$Subject_Fin),1,0)
ETC$SATATIONFLAG<-ifelse (grepl("SATATION",ETC$Subject_Fin),1,0)
ETC$SAVEFLAG<-ifelse (grepl("SAVE",ETC$Subject_Fin),1,0)
ETC$SAVFLAG<-ifelse (grepl("SAV",ETC$Subject_Fin),1,0)
ETC$SAWFLAG<-ifelse (grepl("SAW",ETC$Subject_Fin),1,0)
ETC$SCANFLAG<-ifelse (grepl("SCAN",ETC$Subject_Fin),1,0)
ETC$SCANNERFLAG<-ifelse (grepl("SCANNER",ETC$Subject_Fin),1,0)
ETC$SCHEDULEFLAG<-ifelse (grepl("SCHEDULE",ETC$Subject_Fin),1,0)
ETC$SCHEDULFLAG<-ifelse (grepl("SCHEDUL",ETC$Subject_Fin),1,0)
ETC$SCHEDULINGFLAG<-ifelse (grepl("SCHEDULING",ETC$Subject_Fin),1,0)
ETC$SCOREBOARDFLAG<-ifelse (grepl("SCOREBOARD",ETC$Subject_Fin),1,0)
ETC$SCRAPFLAG<-ifelse (grepl("SCRAP",ETC$Subject_Fin),1,0)
ETC$SCREENFLAG<-ifelse (grepl("SCREEN",ETC$Subject_Fin),1,0)
ETC$SEARCHFLAG<-ifelse (grepl("SEARCH",ETC$Subject_Fin),1,0)
ETC$SEATFLAG<-ifelse (grepl("SEAT",ETC$Subject_Fin),1,0)
ETC$SECURITYFLAG<-ifelse (grepl("SECURITY",ETC$Subject_Fin),1,0)
ETC$SENDFLAG<-ifelse (grepl("SEND",ETC$Subject_Fin),1,0)
ETC$SENSORFLAG<-ifelse (grepl("SENSOR",ETC$Subject_Fin),1,0)
ETC$SERVERFLAG<-ifelse (grepl("SERVER",ETC$Subject_Fin),1,0)
ETC$SERVICEDESKFLAG<-ifelse (grepl("SERVICEDESK",ETC$Subject_Fin),1,0)
ETC$SERVICEFLAG<-ifelse (grepl("SERVICE",ETC$Subject_Fin),1,0)
ETC$SETFLAG<-ifelse (grepl("SET",ETC$Subject_Fin),1,0)
ETC$SETTINGFLAG<-ifelse (grepl("SETTING",ETC$Subject_Fin),1,0)
ETC$SETUPFLAG<-ifelse (grepl("SETUP",ETC$Subject_Fin),1,0)
ETC$SETUPFLAG2<-ifelse (grepl("SET UP",ETC$Subject_Fin),1,0)
ETC$SEVERALFLAG<-ifelse (grepl("SEVERAL",ETC$Subject_Fin),1,0)
ETC$SFDCFLAG<-ifelse (grepl("SFDC",ETC$Subject_Fin),1,0)
ETC$SHAREFLAG<-ifelse (grepl("SHARE",ETC$Subject_Fin),1,0)
ETC$SHAREPOINTFLAG<-ifelse (grepl("SHAREPOINT",ETC$Subject_Fin),1,0)
ETC$SHEETFLAG<-ifelse (grepl("SHEET",ETC$Subject_Fin),1,0)
ETC$SHIPFLAG<-ifelse (grepl("SHIP",ETC$Subject_Fin),1,0)
ETC$SHMFLAG<-ifelse (grepl("SHM",ETC$Subject_Fin),1,0)
ETC$SHOWFLAG<-ifelse (grepl("SHOW",ETC$Subject_Fin),1,0)
ETC$SHOWINGFLAG<-ifelse (grepl("SHOWING",ETC$Subject_Fin),1,0)
ETC$SHUTFLAG<-ifelse (grepl("SHUT",ETC$Subject_Fin),1,0)
ETC$SHUTSDOWNFLAG<-ifelse (grepl("SHUTS DOWN",ETC$Subject_Fin),1,0)
ETC$SHUTSOFFFLAG<-ifelse (grepl("SHUTS OFF",ETC$Subject_Fin),1,0)
ETC$SHUTTERSFLAG<-ifelse (grepl("SHUTTERS",ETC$Subject_Fin),1,0)
ETC$SHUTTINGDOWNFLAG<-ifelse (grepl("SHUTTING DOWN",ETC$Subject_Fin),1,0)
ETC$SIDEWAYSFLAG<-ifelse (grepl("SIDE WAYS",ETC$Subject_Fin),1,0)
ETC$SIGNALFLAG<-ifelse (grepl("SIGNAL",ETC$Subject_Fin),1,0)
ETC$SILHOUETTEFLAG<-ifelse (grepl("SILHOUETTE",ETC$Subject_Fin),1,0)
ETC$SILKFLAG<-ifelse (grepl("SILK",ETC$Subject_Fin),1,0)
ETC$SITEFLAG<-ifelse (grepl("SITE",ETC$Subject_Fin),1,0)
ETC$SITESFLAG<-ifelse (grepl("SITES",ETC$Subject_Fin),1,0)
ETC$SKYPEFLAG<-ifelse (grepl("SKYPE",ETC$Subject_Fin),1,0)
ETC$SLIPFLAG<-ifelse (grepl("SLIP",ETC$Subject_Fin),1,0)
ETC$SLOWFLAG<-ifelse (grepl("SLOW",ETC$Subject_Fin),1,0)
ETC$SLOWLYFLAG<-ifelse (grepl("SLOWLY",ETC$Subject_Fin),1,0)
ETC$SNAPFLAG<-ifelse (grepl("SNAP",ETC$Subject_Fin),1,0)
ETC$SNCFLAG<-ifelse (grepl("SNC",ETC$Subject_Fin),1,0)
ETC$SNMFLAG<-ifelse (grepl("SNM",ETC$Subject_Fin),1,0)
ETC$SOFTWAREFLAG<-ifelse (grepl("SOFTWARE",ETC$Subject_Fin),1,0)
ETC$SOLIDWORKFLAG<-ifelse (grepl("SOLIDWORK",ETC$Subject_Fin),1,0)
ETC$SOMEONEFLAG<-ifelse (grepl("SOMEONE",ETC$Subject_Fin),1,0)
ETC$SORTFLAG<-ifelse (grepl("SORT",ETC$Subject_Fin),1,0)
ETC$SPACEFLAG<-ifelse (grepl("SPACE",ETC$Subject_Fin),1,0)
ETC$SPAMFLAG<-ifelse (grepl("SPAM",ETC$Subject_Fin),1,0)
ETC$SPYWAREFLAG<-ifelse (grepl("SPYWARE",ETC$Subject_Fin),1,0)
ETC$SQFLAG<-ifelse (grepl("SQ",ETC$Subject_Fin),1,0)
ETC$SQLFLAG<-ifelse (grepl("SQL",ETC$Subject_Fin),1,0)
ETC$SRXFLAG<-ifelse (grepl("SRX",ETC$Subject_Fin),1,0)
ETC$STACKFLAG<-ifelse (grepl("STACK",ETC$Subject_Fin),1,0)
ETC$STAIRFLAG<-ifelse (grepl("STAIR",ETC$Subject_Fin),1,0)
ETC$STALLFLAG<-ifelse (grepl("STALL",ETC$Subject_Fin),1,0)
ETC$STATIONFLAG<-ifelse (grepl("STATION",ETC$Subject_Fin),1,0)
ETC$STATUSFLAG<-ifelse (grepl("STATUS",ETC$Subject_Fin),1,0)
ETC$STEPFLAG<-ifelse (grepl("STEP",ETC$Subject_Fin),1,0)
ETC$STILEFLAG<-ifelse (grepl("STILE",ETC$Subject_Fin),1,0)
ETC$STOPFLAG<-ifelse (grepl("STOP",ETC$Subject_Fin),1,0)
ETC$STOPPEDWORKFLAG<-ifelse (grepl("STOPPED WORK",ETC$Subject_Fin),1,0)
ETC$STORAGEFLAG<-ifelse (grepl("STORAGE",ETC$Subject_Fin),1,0)
ETC$STRIPSFLAG<-ifelse (grepl("STRIPS",ETC$Subject_Fin),1,0)
ETC$STUCKFLAG<-ifelse (grepl("STUCK",ETC$Subject_Fin),1,0)
ETC$SUBMITFLAG<-ifelse (grepl("SUBMIT",ETC$Subject_Fin),1,0)
ETC$SUCKFLAG<-ifelse (grepl("SUCK",ETC$Subject_Fin),1,0)
ETC$SUNRISEFLAG<-ifelse (grepl("SUNRISE",ETC$Subject_Fin),1,0)
ETC$SUPERVISORFLAG<-ifelse (grepl("SUPERVISOR",ETC$Subject_Fin),1,0)
ETC$SUPPLYFLAG<-ifelse (grepl("SUPPLY",ETC$Subject_Fin),1,0)
ETC$SURGEFLAG<-ifelse (grepl("SURGE",ETC$Subject_Fin),1,0)
ETC$SURVEYFLAG<-ifelse (grepl("SURVEY",ETC$Subject_Fin),1,0)
ETC$SVNFLAG<-ifelse (grepl("SVN",ETC$Subject_Fin),1,0)
ETC$SWAPFLAG<-ifelse (grepl("SWAP",ETC$Subject_Fin),1,0)
ETC$SWATCHFLAG<-ifelse (grepl("SWATCH",ETC$Subject_Fin),1,0)
ETC$SWIFTPAGEFLAG<-ifelse (grepl("SWIFTPAGE",ETC$Subject_Fin),1,0)
ETC$SWITCHFLAG<-ifelse (grepl("SWITCH",ETC$Subject_Fin),1,0)
ETC$SYNCFLAG<-ifelse (grepl("SYNC",ETC$Subject_Fin),1,0)
ETC$SYSTEMFLAG<-ifelse (grepl("SYSTEM",ETC$Subject_Fin),1,0)
ETC$TABLEFLAG<-ifelse (grepl("TABLE",ETC$Subject_Fin),1,0)
ETC$TABLETFLAG<-ifelse (grepl("TABLET",ETC$Subject_Fin),1,0)
ETC$TAKECALLFLAG<-ifelse (grepl("TAKE CALL",ETC$Subject_Fin),1,0)
ETC$TAKEFLAG<-ifelse (grepl("TAKE",ETC$Subject_Fin),1,0)
ETC$TALYSTFLAG<-ifelse (grepl("TALYST",ETC$Subject_Fin),1,0)
ETC$TAXFLAG<-ifelse (grepl("TAX",ETC$Subject_Fin),1,0)
ETC$TEAMCENTERFLAG<-ifelse (grepl("TEAMCENTER",ETC$Subject_Fin),1,0)
ETC$TEAMFLAG<-ifelse (grepl("TEAM",ETC$Subject_Fin),1,0)
ETC$TEAMVIEWERFLAG<-ifelse (grepl("TEAM VIEWER",ETC$Subject_Fin),1,0)
ETC$TEAMVIEWERFLAG2<-ifelse (grepl("TEAMVIEWER",ETC$Subject_Fin),1,0)
ETC$TECHFLAG<-ifelse (grepl("TECH",ETC$Subject_Fin),1,0)
ETC$TELNETFLAG<-ifelse (grepl("TELNET",ETC$Subject_Fin),1,0)
ETC$TERMINATIONFLAG<-ifelse (grepl("TERMINATION",ETC$Subject_Fin),1,0)
ETC$TESTFLAG<-ifelse (grepl("TEST",ETC$Subject_Fin),1,0)
ETC$THEYFLAG<-ifelse (grepl("THEY",ETC$Subject_Fin),1,0)
ETC$THINKFLAG<-ifelse (grepl("THINK",ETC$Subject_Fin),1,0)
ETC$THRESHOLDFLAG<-ifelse (grepl("THRESHOLD",ETC$Subject_Fin),1,0)
ETC$THRUFLAG<-ifelse (grepl("THRU",ETC$Subject_Fin),1,0)
ETC$TICKETFLAG<-ifelse (grepl("TICKET",ETC$Subject_Fin),1,0)
ETC$TIGERSTOPFLAG<-ifelse (grepl("TIGER STOP",ETC$Subject_Fin),1,0)
ETC$TIMEFLAG<-ifelse (grepl("TIME",ETC$Subject_Fin),1,0)
ETC$TOMCATFLAG<-ifelse (grepl("TOMCAT",ETC$Subject_Fin),1,0)
ETC$TONERFLAG<-ifelse (grepl("TONER",ETC$Subject_Fin),1,0)
ETC$TOOLFLAG<-ifelse (grepl("TOOL",ETC$Subject_Fin),1,0)
ETC$TOUCHSCREENFLAG<-ifelse (grepl("TOUCHSCREEN",ETC$Subject_Fin),1,0)
ETC$TOUCHSCREENFLAG2<-ifelse (grepl("TOUCH SCREEN",ETC$Subject_Fin),1,0)
ETC$TOWERFLAG<-ifelse (grepl("TOWER",ETC$Subject_Fin),1,0)
ETC$TRACKFLAG<-ifelse (grepl("TRACK",ETC$Subject_Fin),1,0)
ETC$TRAFFICFLAG<-ifelse (grepl("TRAFFIC",ETC$Subject_Fin),1,0)
ETC$TRAININGFLAG<-ifelse (grepl("TRAINING",ETC$Subject_Fin),1,0)
ETC$TRANSACTIONFLAG<-ifelse (grepl("TRANSACTION",ETC$Subject_Fin),1,0)
ETC$TRANSFERFLAG<-ifelse (grepl("TRANSFER",ETC$Subject_Fin),1,0)
ETC$TRANSLATORFLAG<-ifelse (grepl("TRANSLATOR",ETC$Subject_Fin),1,0)
ETC$TRANSULATORFLAG<-ifelse (grepl("TRANSULATOR",ETC$Subject_Fin),1,0)
ETC$TRASULATORFLAG<-ifelse (grepl("TRASULATOR",ETC$Subject_Fin),1,0)
ETC$TREEFLAG<-ifelse (grepl("TREE",ETC$Subject_Fin),1,0)
ETC$TRIAGEFLAG<-ifelse (grepl("TRIAGE",ETC$Subject_Fin),1,0)
ETC$TROUBLEFLAG<-ifelse (grepl("TROUBLE",ETC$Subject_Fin),1,0)
ETC$TS3FLAG<-ifelse (grepl("TS3",ETC$Subject_Fin),1,0)
ETC$TUNNELFLAG<-ifelse (grepl("TUNNEL",ETC$Subject_Fin),1,0)
ETC$TURNINGONFLAG<-ifelse (grepl("TURNING ON",ETC$Subject_Fin),1,0)
ETC$TURNONFLAG<-ifelse (grepl("TURN ON",ETC$Subject_Fin),1,0)
ETC$TXMAPD01FLAG<-ifelse (grepl("TXMAPD01",ETC$Subject_Fin),1,0)
ETC$UBABLEFLAG<-ifelse (grepl("UBABLE",ETC$Subject_Fin),1,0)
ETC$UCCXFLAG<-ifelse (grepl("UCCX",ETC$Subject_Fin),1,0)
ETC$UNABLEFLAG<-ifelse (grepl("UNABLE",ETC$Subject_Fin),1,0)
ETC$UNABLLEFLAG<-ifelse (grepl("UNABLLE",ETC$Subject_Fin),1,0)
ETC$UNAVAILABLEFLAG<-ifelse (grepl("UNAVAILABLE",ETC$Subject_Fin),1,0)
ETC$UNAVAILABILITYFLAG<-ifelse (grepl("UNAVAILABILITY",ETC$Subject_Fin),1,0)
ETC$UNITFLAG<-ifelse (grepl("UNIT",ETC$Subject_Fin),1,0)
ETC$UNKNOWNFLAG<-ifelse (grepl("UNKNOWN",ETC$Subject_Fin),1,0)
ETC$UNPLUGGEDFLAG<-ifelse (grepl("UNPLUGGED",ETC$Subject_Fin),1,0)
ETC$UPDATFLAG<-ifelse (grepl("UPDAT",ETC$Subject_Fin),1,0)
ETC$UPDATEFLAG<-ifelse (grepl("UPDATE",ETC$Subject_Fin),1,0)
ETC$UPFLAG<-ifelse (grepl("UP",ETC$Subject_Fin),1,0)
ETC$UPGRADFLAG<-ifelse (grepl("UPGRAD",ETC$Subject_Fin),1,0)
ETC$UPGRADEFLAG<-ifelse (grepl("UPGRADE",ETC$Subject_Fin),1,0)
ETC$UPLINKFLAG<-ifelse (grepl("UPLINK",ETC$Subject_Fin),1,0)
ETC$UPLOADFLAG<-ifelse (grepl("UPLOAD",ETC$Subject_Fin),1,0)
ETC$UPSFLAG<-ifelse (grepl("UPS",ETC$Subject_Fin),1,0)
ETC$UPSIDEDOWNFLAG<-ifelse (grepl("UPSIDE DOWN",ETC$Subject_Fin),1,0)
ETC$URLFLAG<-ifelse (grepl("URL",ETC$Subject_Fin),1,0)
ETC$USAGEFLAG<-ifelse (grepl("USAGE",ETC$Subject_Fin),1,0)
ETC$USBFLAG<-ifelse (grepl("USB",ETC$Subject_Fin),1,0)
ETC$USERFLAG<-ifelse (grepl("USER",ETC$Subject_Fin),1,0)
ETC$USERSFLAG<-ifelse (grepl("USERS",ETC$Subject_Fin),1,0)
ETC$VALANCEFLAG<-ifelse (grepl("VALANCE",ETC$Subject_Fin),1,0)
ETC$VENDORFLAG<-ifelse (grepl("VENDOR",ETC$Subject_Fin),1,0)
ETC$VERIFYFLAG<-ifelse (grepl("VERIFY",ETC$Subject_Fin),1,0)
ETC$VIEWFLAG<-ifelse (grepl("VIEW",ETC$Subject_Fin),1,0)
ETC$VIGNETTEFLAG<-ifelse (grepl("VIGNETTE",ETC$Subject_Fin),1,0)
ETC$VIRTUALMACHINEFLAG<-ifelse (grepl("VIRTUAL MACHINE",ETC$Subject_Fin),1,0)
ETC$VIRUSFLAG<-ifelse (grepl("VIRUS",ETC$Subject_Fin),1,0)
ETC$VISIBLEFLAG<-ifelse (grepl("VISIBLE",ETC$Subject_Fin),1,0)
ETC$VISIOFLAG<-ifelse (grepl("VISIO",ETC$Subject_Fin),1,0)
ETC$VLANFLAG<-ifelse (grepl("VLAN",ETC$Subject_Fin),1,0)
ETC$VMFLAG<-ifelse (grepl("VM",ETC$Subject_Fin),1,0)
ETC$VMWAREFLAG<-ifelse (grepl("VMWARE",ETC$Subject_Fin),1,0)
ETC$VOICEMAILFLAG<-ifelse (grepl("VOICEMAIL",ETC$Subject_Fin),1,0)
ETC$VOICEMAILFLAG2<-ifelse (grepl("VOICE MAIL",ETC$Subject_Fin),1,0)
ETC$VOIPFLAG<-ifelse (grepl("VOIP",ETC$Subject_Fin),1,0)
ETC$VOLUMEFLAG<-ifelse (grepl("VOLUME",ETC$Subject_Fin),1,0)
ETC$VPNFLAG<-ifelse (grepl("VPN",ETC$Subject_Fin),1,0)
ETC$WARNINGFLAG<-ifelse (grepl("WARNING",ETC$Subject_Fin),1,0)
ETC$WARRANTYFLAG<-ifelse (grepl("WARRANTY",ETC$Subject_Fin),1,0)
ETC$WASPFLAG<-ifelse (grepl("WASP",ETC$Subject_Fin),1,0)
ETC$WATERVLIETFLAG<-ifelse (grepl("WATERVLIET",ETC$Subject_Fin),1,0)
ETC$WEBFLAG<-ifelse (grepl("WEB",ETC$Subject_Fin),1,0)
ETC$WEBEXFLAG<-ifelse (grepl("WEBEX",ETC$Subject_Fin),1,0)
ETC$WEBEXFLAG2<-ifelse (grepl("WEB EX",ETC$Subject_Fin),1,0)
ETC$WEBSITEFLAG<-ifelse (grepl("WEBSITE",ETC$Subject_Fin),1,0)
ETC$WECANTLOGINFLAG<-ifelse (grepl("WE CANT LOG IN",ETC$Subject_Fin),1,0)
ETC$WEFLAG<-ifelse (grepl("WE ",ETC$Subject_Fin),1,0)
ETC$WGFLAG<-ifelse (grepl("WG",ETC$Subject_Fin),1,0)
ETC$WHENFLAG<-ifelse (grepl("WHEN",ETC$Subject_Fin),1,0)
ETC$WIFIFLAG<-ifelse (grepl("WIFI",ETC$Subject_Fin),1,0)
ETC$WIKIFLAG<-ifelse (grepl("WIKI",ETC$Subject_Fin),1,0)
ETC$WILEYFLAG<-ifelse (grepl("WILEY",ETC$Subject_Fin),1,0)
ETC$WILLNOTFLAG<-ifelse (grepl("WILL NOT",ETC$Subject_Fin),1,0)
ETC$WILLNOTTAKEFLAG<-ifelse (grepl("WILL NOT TAKE",ETC$Subject_Fin),1,0)
ETC$WINFLAG<-ifelse (grepl("WIN",ETC$Subject_Fin),1,0)
ETC$WINDOWSFLAG<-ifelse (grepl("WINDOWS",ETC$Subject_Fin),1,0)
ETC$WINZIPFLAG<-ifelse (grepl("WINZIP",ETC$Subject_Fin),1,0)
ETC$WIRELESSFLAG<-ifelse (grepl("WIRELESS",ETC$Subject_Fin),1,0)
ETC$WITHOUTFLAG<-ifelse (grepl("WITHOUT",ETC$Subject_Fin),1,0)
ETC$WONTFEEDFLAG<-ifelse (grepl("WONT FEED",ETC$Subject_Fin),1,0)
ETC$WONTFLAG<-ifelse (grepl("WONT",ETC$Subject_Fin),1,0)
ETC$WONTHOLDFLAG<-ifelse (grepl("WONT HOLD",ETC$Subject_Fin),1,0)
ETC$WONTWORKFLAG<-ifelse (grepl("WONT WORK",ETC$Subject_Fin),1,0)
ETC$WOODFLAG<-ifelse (grepl("WOOD",ETC$Subject_Fin),1,0)
ETC$WORKFLAG<-ifelse (grepl("WORK",ETC$Subject_Fin),1,0)
ETC$WORKINGFLAG<-ifelse (grepl("WORKING",ETC$Subject_Fin),1,0)
ETC$WRAPFLAG<-ifelse (grepl("WRAP",ETC$Subject_Fin),1,0)
ETC$WRITEFLAG<-ifelse (grepl("WRITE",ETC$Subject_Fin),1,0)
ETC$WRONGFLAG<-ifelse (grepl("WRONG",ETC$Subject_Fin),1,0)
ETC$WYSETERMINALSFLAG<-ifelse (grepl("WYSE TERMINALS",ETC$Subject_Fin),1,0)
ETC$XMLFLAG<-ifelse (grepl("XML",ETC$Subject_Fin),1,0)
ETC$ZEBRAFLAG<-ifelse (grepl("ZEBRA",ETC$Subject_Fin),1,0)
ETC$ZEEBRAFLAG<-ifelse (grepl("ZEEBRA",ETC$Subject_Fin),1,0)
ETC$ZENWORKSFLAG<-ifelse (grepl("ZENWORKS",ETC$Subject_Fin),1,0)
ETC$ZIPFLAG<-ifelse (grepl("ZIP",ETC$Subject_Fin),1,0)


## If else statement to filter the "1" different Keyword columns ##

## with AND option ##

ETC$EMAILFLAG   <- ifelse (ETC$EMAILFLAG > 0 | ETC$EMAILFLAG2 > 0 | ETC$EMAILFLAG3 > 0 | ETC$EMAILFLAG4 > 0,1,0)
ETC$FREEZEFLAG  <- ifelse (ETC$FREEZEDFLAG > 0 | ETC$FREEZESFLAG > 0 | ETC$FREEZESSFLAG > 0 | ETC$FREEZINGFLAG > 0 |ETC$FROZEFLAG > 0 | ETC$FROZENFLAG > 0 | ETC$FREEZFLAG > 0,1,0)
ETC$UNABLEFLAG  <- ifelse (ETC$UNABLEFLAG > 0 | ETC$UNABLLEFLAG > 0 | ETC$UBABLEFLAG > 0 | ETC$NOTABLEFLAG > 0 | ETC$NOTWORKFLAG > 0,1,0)
ETC$LABLEFLAG   <- ifelse (ETC$LABLEFLAG > 0 & ETC$AVAILABLEFLAG > 0,0,paste(ETC$LABLEFLAG))  
ETC$LABELFLAG   <- ifelse (ETC$LABELFLAG > 0 | ETC$LABLEFLAG > 0,1,0)
ETC$LOGOUTFLAG  <- ifelse (ETC$LOGSOFFFLAG > 0 | ETC$LOGOUTFLAG > 0,1,0)
ETC$LOGINFLAG   <- ifelse (ETC$LOGINFLAG > 0 | ETC$LOGINFLAG2 > 0 | ETC$LOGONFLAG > 0 | ETC$LOGONFLAG2 > 0 | ETC$LOGGINGFLAG > 0,1,0)
ETC$PERFORMANCEFLAG <- ifelse (ETC$PERFORMANCEFLAG > 0 | ETC$PERFORMINGFLAG > 0,1,0)
ETC$TRANSLATORFLAG  <- ifelse (ETC$TRANSLATORFLAG > 0 | ETC$TRANSULATORFLAG > 0 | ETC$TRASULATORFLAG > 0,1,0)
ETC$RESTARTEDFLAG   <- ifelse (ETC$RESTARTEDFLAG > 0 | ETC$RESTARTINGFLAG > 0,1,0)
ETC$BACKINGFLAG     <- ifelse (ETC$BACKINGFLAG > 0 | ETC$BACKINFLAG > 0 | ETC$BACKINGUPFLAG > 0,1,0)
ETC$DOWNFLAG    <- ifelse (ETC$DOWNFLAG > 0 & (ETC$DOWNLOADFLAG > 0 | ETC$DOWNSTAIRFLAG > 0 | ETC$DROPFLAG > 0 | ETC$STAIRFLAG > 0),0,paste(ETC$DOWNFLAG))
ETC$NOTFLAG     <- ifelse (ETC$NOTFLAG > 0 & (ETC$NOTESFLAG > 0 | ETC$NOTICEFLAG > 0),0,paste(ETC$NOTFLAG))
ETC$NOFLAG      <- ifelse (ETC$NOFLAG > 0 & (ETC$NOTESFLAG > 0 | ETC$KNOWFLAG > 0 | ETC$NONFLAG > 0),0,paste(ETC$NOFLAG))
ETC$NEWFLAG     <- ifelse (ETC$NEWFLAG > 0 & ETC$RENEWFLAG > 0,0,paste(ETC$NEWFLAG))
ETC$STALLFLAG   <- ifelse (ETC$STALLFLAG > 0 & ETC$INSTALLFLAG > 0,0,paste(ETC$STALLFLAG))
ETC$FRAMEFLAG   <- ifelse (ETC$FRAMEFLAG > 0 & ETC$PLATEFLAG > 0,0,paste(ETC$FRAMEFLAG))
ETC$USERSFLAG   <- ifelse (ETC$USERSFLAG > 0 | ETC$NONEOFTHEUSERFLAG > 0 | ETC$NONOFTHEUSERFLAG > 0 | ETC$MANYUSERFLAG > 0 | ETC$ENTIREFLOORFLAG > 0 | ETC$COMPUTERSFLAG > 0 | ETC$ENTIRETEAMFLAG > 0,1,0)
ETC$RESETFLAG   <- ifelse (ETC$RESETFLAG > 0 | ETC$RESETFLAG2 > 0,1,0)
ETC$PASSWORDFLAG   <- ifelse (ETC$PASSWORDFLAG > 0 | ETC$PWFLAG > 0,1,0)
ETC$VOICEMAILFLAG  <- ifelse (ETC$VOICEMAILFLAG > 0 | ETC$VOICEMAILFLAG2 > 0,1,0)
ETC$HEADSETFLAG    <- ifelse (ETC$HEADSETFLAG > 0 | ETC$HEADSETFLAG2 > 0,1,0)
ETC$CANNOTFLAG     <- ifelse (ETC$CANNOTFLAG > 0 | ETC$CANNOTFLAG2 > 0,1,0)
ETC$BARCODEFLAG    <- ifelse (ETC$BARCODEFLAG > 0 | ETC$BARCODEFLAG2 > 0,1,0)
ETC$TOUCHSCREENFLAG <- ifelse (ETC$TOUCHSCREENFLAG > 0 | ETC$TOUCHSCREENFLAG2 > 0,1,0)
ETC$INKFLAG       <- ifelse (ETC$INKFLAG > 0 & (ETC$THINKFLAG > 0 | ETC$LINKFLAG > 0),0,paste(ETC$INKFLAG))
ETC$INTERNFLAG    <- ifelse (ETC$INTERNFLAG > 0 & (ETC$INTERNETFLAG > 0 | ETC$INTERNALFLAG > 0),0,paste(ETC$INTERNFLAG))
ETC$ROLLERFLAG    <- ifelse (ETC$ROLLERFLAG > 0 & ETC$CONTROLLERFLAG > 0,0,paste(ETC$ROLLERFLAG))
ETC$DIRECTWEBFLAG <- ifelse (ETC$DIRECTWEBFLAG > 0 | ETC$DIRECTWEBFLAG2 > 0,1,0)
ETC$ININFLAG      <- ifelse (ETC$ININFLAG > 0 & ETC$TRAININGFLAG > 0,0,paste(ETC$ININFLAG))
ETC$LINKFLAG      <- ifelse (ETC$LINKFLAG > 0 & ETC$LINKAGFLAG > 0,0,paste(ETC$LINKFLAG))
ETC$AREFLAG       <- ifelse (ETC$AREFLAG > 0 & ETC$AREAFLAG > 0,0,paste(ETC$AREFLAG))
ETC$ALLFLAG       <- ifelse (ETC$ALLFLAG > 0 & (ETC$ALLANFLAG > 0 | ETC$ALLOWFLAG > 0),0,paste(ETC$ALLFLAG))
ETC$SAPFLAG       <- ifelse (ETC$SAPFLAG > 0 & ETC$ASAPFLAG > 0,0,paste(ETC$SAPFLAG))
ETC$SETUPFLAG     <- ifelse (ETC$SETUPFLAG > 0 | ETC$SETUPFLAG2 > 0,1,0)
ETC$OFFLINEFLAG   <- ifelse (ETC$OFFLINEFLAG > 0 | ETC$OFFLINEFLAG2 > 0,1,0)
ETC$WEBEXFLAG     <- ifelse (ETC$WEBEXFLAG > 0 | ETC$WEBEXFLAG2 > 0,1,0)
ETC$HELPDESKFLAG  <- ifelse (ETC$HELPDESKFLAG > 0 | ETC$HELPDESKFLAG2 > 0,1,0)
ETC$ITSUPPORTFLAG <- ifelse (ETC$ITSUPPORTFLAG > 0 | ETC$ITSUPPORTFLAG2 > 0,1,0)


## If else statements in Keywordvar column ##

ETC$KEYWORDVAR <- "NA"

ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	EMAILFLAG  > 0          
	,"AAA01 PASSWORD RESET EMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	EMAILFLAG  > 0          
	,"AAA02 PASSWORD EMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	EXCHANGEFLAG  > 0          
	,"AAA03 PASSWORD RESET EXCHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	EXCHANGEFLAG  > 0          
	,"AAA04 PASSWORD EXCHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	NETWORKFLAG  > 0          
	,"AAA05 PASSWORD RESET NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	NETWORKFLAG  > 0          
	,"AAA06 PASSWORD NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	SAPFLAG  > 0          
	,"AAA07 PASSWORD RESET SAP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	SAPFLAG  > 0          
	,"AAA08 PASSWORD SAP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	WEBEXFLAG  > 0          
	,"AAA09 PASSWORD RESET WEBEX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	WEBEXFLAG  > 0          
	,"AAA10 PASSWORD WEBEX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	COMPUTERFLAG  > 0          
	,"AAA11 PASSWORD RESET COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	COMPUTERFLAG  > 0          
	,"AAA12 PASSWORD COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	WINDOWSFLAG  > 0          
	,"AAA13 PASSWORD RESET WINDOWS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	WINDOWSFLAG  > 0          
	,"AAA14 PASSWORD WINDOWS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	PHONEFLAG  > 0          
	,"AAA15 PASSWORD RESET PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	PHONEFLAG  > 0          
	,"AAA16 PASSWORD PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	VOICEMAILFLAG  > 0          
	,"AAA17 PASSWORD RESET VOICEMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	VOICEMAILFLAG  > 0          
	,"AAA18 PASSWORD VOICEMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	ACCOUNTFLAG  > 0          
	,"AAA19 PASSWORD RESET ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	ACCOUNTFLAG  > 0          
	,"AAA20 PASSWORD ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	INTERNETFLAG  > 0          
	,"AAA21 PASSWORD RESET INTERNET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	INTERNETFLAG  > 0          
	,"AAA22 PASSWORD INTERNET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	USERFLAG  > 0          
	,"AAA23 PASSWORD RESET USER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	USERFLAG  > 0          
	,"AAA24 PASSWORD USER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	ADPFLAG  > 0          
	,"AAA25 PASSWORD RESET ADP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	ADPFLAG  > 0          
	,"AAA26 PASSWORD ADP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	VPNFLAG  > 0          
	,"AAA27 PASSWORD RESET VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	VPNFLAG  > 0          
	,"AAA28 PASSWORD VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	ASTEAFLAG  > 0          
	,"AAA29 PASSWORD RESET ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	ASTEAFLAG  > 0          
	,"AAA30 PASSWORD ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	SFDCFLAG  > 0          
	,"AAA31 PASSWORD RESET SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	SFDCFLAG  > 0          
	,"AAA32 PASSWORD SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	ITCHECKFLAG  > 0          
	,"AAA33 PASSWORD RESET ITCHECK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	ITCHECKFLAG  > 0          
	,"AAA34 PASSWORD ITCHECK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	PCARDFLAG  > 0          
	,"AAA35 PASSWORD RESET PCARD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	PCARDFLAG  > 0          
	,"AAA36 PASSWORD PCARD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	PCFLAG  > 0          
	,"AAA37 PASSWORD RESET PC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	PCFLAG  > 0          
	,"AAA38 PASSWORD PC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	TS3FLAG  > 0          
	,"AAA39 PASSWORD RESET TS3",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	TS3FLAG  > 0          
	,"AAA40 PASSWORD TS3",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	FISFLAG  > 0          
	,"AAA41 PASSWORD RESET FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	FISFLAG  > 0          
	,"AAA42 PASSWORD FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	BWFLAG  > 0          
	,"AAA43 PASSWORD RESET BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	BWFLAG  > 0          
	,"AAA44 PASSWORD BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0) & ETC$
	AEFLAG  > 0          
	,"AAA45 PASSWORD RESET AE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & ETC$
	AEFLAG  > 0          
	,"AAA46 PASSWORD AE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$AUTOFLAG  > 0)          
	,"AAA47 PASSWORD RESET AUTO REPLY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	ETC$REMINDFLAG  > 0          
	,"AAA48 PASSWORD RESET AUTO",        paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0 & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$RELEASEFLAG  > 0| ETC$
	NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	UPDATEFLAG  > 0| ETC$INVALIDFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$
	CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$LOGINFLAG  > 0| ETC$NEWFLAG  > 0| ETC$
	LOSTFLAG  > 0| ETC$FAILFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$HELPFLAG  > 0| ETC$AUTOFLAG  > 0| ETC$REMINDFLAG  > 0)          
	,"AAA49 PASSWORD RESET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PASSWORDFLAG  > 0                                        
	,"AAA99 PASSWORD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAB01 SAP ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0)          
	,"AAB02 SAP DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0)          
	,"AAB03 SAP LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$TRAININGFLAG  > 0)          
	,"AAB04 SAP TRAINING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$SERVERFLAG  > 0)          
	,"AAB05 SAP SERVER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$REQUESTFLAG  > 0) & ETC$NEWFLAG  > 0          
	,"AAB06 SAP NEW REQUEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0) & ETC$
	NEWFLAG  > 0         
	,"AAB07 SAP NEW ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAB08 SAP ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$PRINTFLAG  > 0)          
	,"AAB09 SAP PRINT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0)          
	,"AAB10 SAP INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$CODEFLAG  > 0)          
	,"AAB10 SAP CODE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$ORDERFLAG  > 0| ETC$TRANSACTIONFLAG  > 0| ETC$INVOICEFLAG  > 0)          
	,"AAB11 SAP| ETC$DER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$INVENTORYFLAG  > 0)          
	,"AAB12 SAP INVENTORY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$ITEMFLAG  > 0)          
	,"AAB13 SAP ITEM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$MATERIALFLAG  > 0)          
	,"AAB14 SAP MATERIAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$REPORTFLAG  > 0)          
	,"AAB15 SAP REPORT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0)          
	,"AAB16 SAP CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$AUTHFLAG  > 0)          
	,"AAB17 SAP AUTH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$RESETFLAG  > 0)          
	,"AAB18 SAP RESET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$SHIPFLAG  > 0)          
	,"AAB19 SAP SHIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$RIGHTSFLAG  > 0)          
	,"AAB20 SAP RIGHTS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$GROUPFLAG  > 0)          
	,"AAB21 SAP GROUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$SETTINGFLAG  > 0)          
	,"AAB22 SAP SETTING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$EXCELFLAG  > 0)          
	,"AAB23 SAP EXCEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$PARTFLAG  > 0)          
	,"AAB24 SAP PART",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$EXPORTFLAG  > 0| ETC$OUTPUTFLAG  > 0)          
	,"AAB25 SAP OUTPUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$DOCUMENTFLAG  > 0)          
	,"AAB26 SAP DOCUMENT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$VPNFLAG  > 0)          
	,"AAB27 SAP VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)          
	,"AAB28 SAP PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$SAVEFLAG  > 0)          
	,"AAB29 SAP SAVE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$EQUIPMENTFLAG  > 0)          
	,"AAB30 SAP EQUIPMENT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$DOCUMENTFLAG  > 0)          
	,"AAB31 SAP DOCUMENT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$PORTALFLAG  > 0)          
	,"AAB32 SAP PORTAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$REFUSOLFLAG  > 0)          
	,"AAB33 SAP REFUSOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$ASTEAFLAG  > 0)          
	,"AAB34 SAP ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$TS3FLAG  > 0)          
	,"AAB35 SAP TS3",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$BOMFLAG  > 0)          
	,"AAB36 SAP BOM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$BWFLAG  > 0)          
	,"AAB37 SAP BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$TOOLFLAG  > 0)          
	,"AAB38 SAP TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$TICKETFLAG  > 0)          
	,"AAB39 SAP TICKET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAB40 SAP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0 & 
	(ETC$REQUESTFLAG  > 0)          
	,"AAB41 SAP REQUEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SAPFLAG  > 0                                        
	,"AAB99 SAP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$DISABLEFLAG  > 0)          
	,"AAC01 WEBEX DISABLE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAC02 WEBEX ACESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0)          
	,"AAC03 WEBEX DOWN NOTWORK",        paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))


ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0)          
	,"AAC04 WEBEX LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0)          
	,"AAC05 WEBEX INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$HOSTFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAC06 WEBEX ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$VPNFLAG  > 0)          
	,"AAC07 WEBEX VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0)          
	,"AAC08 WEBEX CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$AUTHFLAG  > 0)          
	,"AAC09 WEBEX AUTH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$SETTINGFLAG  > 0)          
	,"AAC10 WEBEX SETTING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0| ETC$CALENDARFLAG  > 0)          
	,"AAC11 WEBEX OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$MEETFLAG  > 0)          
	,"AAC12 WEBEX MEETING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$ASTEAFLAG  > 0)          
	,"AAC13 WEBEX ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$TOOLFLAG  > 0)          
	,"AAC14 WEBEX TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAC15 WEBEX ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$REQUESTFLAG  > 0)          
	,"AAC16 WEBEX REQUEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0 & 
	(ETC$CONNECTFLAG  > 0)          
	,"AAC17 WEBEX CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBEXFLAG  > 0                                        
	,"AAC99 WEBEX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$REMOVFLAG  > 0| ETC$DISABLEFLAG  > 0)          
	,"AAD01 VOICEMAIL DISABLE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAD02 VOICEMAIL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0)          
	,"AAD03 VOICEMAIL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0)          
	,"AAD04 VOICEMAIL LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$HOSTFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAD05 VOICEMAIL ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0)          
	,"AAD06 VOICEMAIL CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOICEMAILFLAG  > 0                                        
	,"AAD99 VOICEMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"AAE01 EMAIL DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAE02 EMAIL ACCESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0)          
	,"AAE03 EMAIL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0)          
	,"AAE04 EMAIL LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"AAE05 EMAIL INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$SERVERFLAG  > 0)          
	,"AAE06 EMAIL SERVER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$HOSTFLAG  > 0| ETC$AGENTIDFLAG  > 0)         
	,"AAE07 EMAIL ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$PRINTFLAG  > 0)          
	,"AAE08 EMAIL PRINT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$SCANFLAG  > 0)          
	,"AAE09 EMAIL SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)          
	,"AAE10 EMAIL PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$COPIERFLAG  > 0)          
	,"AAE11 EMAIL COPIER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"AAE12 EMAIL CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$LOSTFLAG  > 0| ETC$RESTORFLAG  > 0)          
	,"AAE13 EMAIL RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$NOTIFFLAG  > 0)          
	,"AAE14 EMAIL NOTIFICATION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$AUTHFLAG  > 0| ETC$PERMISSIONFLAG  > 0)          
	,"AAE15 EMAIL AUTH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$GROUPFLAG  > 0)          
	,"AAE16 EMAIL GROUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0)          
	,"AAE17 EMAIL VIRUS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$HACKFLAG  > 0)          
	,"AAE18 EMAIL HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$JUNKFLAG  > 0)          
	,"AAE19 EMAIL JUNK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0| ETC$MICROSOFTFLAG  > 0)          
	,"AAE20 EMAIL OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$TOOLFLAG  > 0)          
	,"AAE21 EMAIL TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$LINKFLAG  > 0)          
	,"AAE22 EMAIL LINK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$TESTFLAG  > 0)          
	,"AAE23 EMAIL TEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$JOBFLAG  > 0)          
	,"AAE24 EMAIL JOB",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAE25 EMAIL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0 & 
	ETC$REQUESTFLAG  > 0          
	,"AAE26 EMAIL REQUEST",             paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EMAILFLAG  > 0                                        
	,"AAE99 EMAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0| ETC$MIGRATFLAG  > 0| ETC$AVAILABLEFLAG  > 0)          
	,"AAF01 MAIL DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAF02 MAIL ACCESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0)          
	,"AAF03 MAIL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$RESETFLAG  > 0)          
	,"AAF04 MAIL LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"AAF05 MAIL INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$SERVERFLAG  > 0)          
	,"AAF06 MAIL SERVER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$HOSTFLAG  > 0| ETC$AGENTIDFLAG  > 0)         
	,"AAF07 MAIL ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$PRINTFLAG  > 0)          
	,"AAF08 MAIL PRINT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$SCANFLAG  > 0)          
	,"AAF09 MAIL SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)          
	,"AAF10 MAIL PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$COPIERFLAG  > 0)          
	,"AAF11 MAIL COPIER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"AAF12 MAIL CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$LOSTFLAG  > 0| ETC$RESTORFLAG  > 0)          
	,"AAF13 MAIL RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$NOTIFFLAG  > 0)          
	,"AAF14 MAIL NOTIFICATION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$AUTHFLAG  > 0| ETC$PERMISSIONFLAG  > 0)          
	,"AAF15 MAIL AUTH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$GROUPFLAG  > 0)          
	,"AAF16 MAIL GROUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0)          
	,"AAF17 MAIL VIRUS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$HACKFLAG  > 0)          
	,"AAF18 MAIL HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$JUNKFLAG  > 0)          
	,"AAF19 MAIL JUNK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0| ETC$MICROSOFTFLAG  > 0)          
	,"AAF20 MAIL OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$TOOLFLAG  > 0)          
	,"AAF21 MAIL TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$LINKFLAG  > 0)          
	,"AAF22 MAIL LINK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$TESTFLAG  > 0)          
	,"AAF23 MAIL TEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$JOBFLAG  > 0)          
	,"AAF24 MAIL JOB",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$FULLFLAG  > 0)          
	,"AAF25 MAIL FULL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAF26 MAIL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAILFLAG  > 0                                        
	,"AAF99 MAIL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0| ETC$OFFLINEFLAG  > 0)          
	,"AAG01 PRINT ACCESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	DELAYFLAG  > 0| ETC$HALFFLAG  > 0| ETC$SIDEWAYSFLAG  > 0| ETC$JAMFLAG  > 0| ETC$ALERTFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$
	CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$NOTAVAILABLEFLAG  > 0) & 
	(ETC$LABELFLAG  > 0| ETC$LABLEFLAG  > 0| ETC$ZEBRAFLAG  > 0| ETC$ZEEBRAFLAG  > 0| ETC$STATIONFLAG  > 0| ETC$SLIPFLAG  > 0| ETC$UPSFLAG  > 0| ETC$PACKFLAG  > 0| ETC$
	FAXFLAG  > 0| ETC$LISTFLAG  > 0| ETC$BARCODEFLAG  > 0 | ETC$WRAPFLAG  > 0| ETC$WASPFLAG  > 0| ETC$SAWFLAG  > 0| ETC$WOODFLAG  > 0)       
	,"AAG02 PRINT DOWN NOTWORK PROD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	DELAYFLAG  > 0| ETC$HALFFLAG  > 0| ETC$SIDEWAYSFLAG  > 0| ETC$JAMFLAG  > 0| ETC$ALERTFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$
	CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$NOTAVAILABLEFLAG  > 0)          
	,"AAG03 PRINT DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$LOGINFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$RESETFLAG  > 0)          
	,"AAG04 PRINT LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"AAG05 PRINT INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$TONERFLAG  > 0)          
	,"AAG06 PRINT TONER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$INKFLAG  > 0| ETC$CARTRIDGEFLAG  > 0)          
	,"AAG07 PRINT INK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$NEWFLAG  > 0)          
	,"AAG08 NEW PRINTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"AAG09 FIX PRINTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$SCANFLAG  > 0)          
	,"AAG10 PRINT SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$AUTOFLAG  > 0)          
	,"AAG11 PRINT AUTO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATFLAG  > 0| ETC$UPGRADFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$
	APPROVFLAG  > 0| ETC$DELETFLAG  > 0| ETC$REBOOTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0| ETC$MAPFLAG  > 0| ETC$DEFAULTFLAG  > 0| ETC$SETFLAG  > 0)          
	,"AAG12 PRINT CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$CANCELFLAG  > 0| ETC$STOPFLAG  > 0)          
	,"AAG13 PRINT CANCEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$JOBFLAG  > 0)          
	,"AAG14 PRINT JOB",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$LABELFLAG  > 0| ETC$LABLEFLAG  > 0| ETC$OUTFLAG  > 0| ETC$SLIPFLAG  > 0| ETC$LISTFLAG  > 0| ETC$BARCODEFLAG  > 0)          
	,"AAG15 PRINT LABEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$INVOICEFLAG  > 0| ETC$OUTFLAG  > 0)          
	,"AAG16 PRINT INVOICE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$ZEBRAFLAG  > 0| ETC$ZEEBRAFLAG  > 0)          
	,"AAG17 PRINT ZEBRA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$SHIPFLAG  > 0)          
	,"AAG18 PRINT SHIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$TRAININGFLAG  > 0)          
	,"AAG19 PRINT TRAINING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAG20 PRINT ISSUE",               paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRINTFLAG  > 0                                        
	,"AAG99 PRINT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	BWFLAG  > 0          
	,"AAH01 LOGIN NOT WORK BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	CISCOFLAG  > 0          
	,"AAH02 LOGIN NOT WORK CISCO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	COMPUTERFLAG  > 0          
	,"AAH03 LOGIN NOT WORK COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	DELLFLAG  > 0          
	,"AAH04 LOGIN NOT WORK DELL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	DOMAINFLAG  > 0          
	,"AAH05 LOGIN NOT WORK DOMAIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	EMPLOYEEFLAG  > 0          
	,"AAH06 LOGIN NOT WORK EMPLOYEE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	EPDMFLAG  > 0          
	,"AAH07 LOGIN NOT WORK EPMD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	EXCHANGEFLAG  > 0          
	,"AAH08 LOGIN NOT WORK EXCHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	EXTRANETFLAG  > 0          
	,"AAH09 LOGIN NOT WORK EXTRANET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	FISFLAG  > 0          
	,"AAH10 LOGIN NOT WORK FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	FUSIONOPSFLAG  > 0          
	,"AAH11 LOGIN NOT WORK FOPS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	INTERNETFLAG  > 0          
	,"AAH12 LOGIN NOT WORK INTERNET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & 
	(ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)        
	,"AAH13 LOGIN NOT WORK LAPTOP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	NETWORKFLAG  > 0        
	,"AAH14 LOGIN NOT WORK NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0)        
	,"AAH15 LOGIN NOT WORK OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	PHONEFLAG  > 0       
	,"AAH16 LOGIN NOT WORK PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	PORTALFLAG  > 0       
	,"AAH17 LOGIN NOT WORK PORTAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	REMOTEFLAG  > 0       
	,"AAH18 LOGIN NOT WORK REMOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	SALESFORCEFLAG  > 0       
	,"AAH19 LOGIN NOT WORK SFORCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	SFDCFLAG  > 0       
	,"AAH20 LOGIN NOT WORK SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	TOMCATFLAG  > 0       
	,"AAH21 LOGIN NOT WORK TOMCAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	TOOLFLAG  > 0       
	,"AAH22 LOGIN NOT WORK TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0) & ETC$
	VPNFLAG  > 0       
	,"AAH23 LOGIN NOT WORK VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0  & 
	(ETC$RESETFLAG  > 0| ETC$EXPIRFLAG  > 0| ETC$LOCKFLAG  > 0| ETC$REQUESTFLAG  > 0| ETC$FORGOTFLAG  > 0| ETC$NOTICEFLAG  > 0| ETC$
	ERRORFLAG  > 0| ETC$FAILFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$WONTFLAG  > 0| ETC$
	DENIEDFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$
	QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$
	INVALIDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$INCORRECTFLAG  > 0| ETC$WRONGFLAG  > 0| ETC$WARNINGFLAG  > 0)           
	,"AAH24 LOGIN NOT WORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAH25 LOGIN ACCESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$LONGFLAG  > 0| ETC$SLOWFLAG  > 0)          
	,"AAH26 LOGIN LONG",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$NEWFLAG  > 0)          
	,"AAH27 NEW LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATFLAG  > 0| ETC$UPGRADFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$
	APPROVFLAG  > 0| ETC$DELETFLAG  > 0| ETC$REBOOTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0| ETC$MAPFLAG  > 0| ETC$DEFAULTFLAG  > 0| ETC$SETFLAG  > 0)          
	,"AAH28 LOGIN CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$TESTFLAG  > 0)          
	,"AAH29 LOGIN TEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAH30 LOGIN ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	BWFLAG  > 0          
	,"AAH31 LOGIN ISSUE BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	CISCOFLAG  > 0          
	,"AAH32 LOGIN ISSUE CISCO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	COMPUTERFLAG  > 0          
	,"AAH33 LOGIN ISSUE COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	DELLFLAG  > 0          
	,"AAH34 LOGIN ISSUE DELL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	DOMAINFLAG  > 0          
	,"AAH35 LOGIN ISSUE DOMAIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	EMPLOYEEFLAG  > 0          
	,"AAH36 LOGIN ISSUE EMPLOYEE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	EPDMFLAG  > 0          
	,"AAH37 LOGIN ISSUE EPMD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	EXCHANGEFLAG  > 0          
	,"AAH38 LOGIN ISSUE EXCHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	EXTRANETFLAG  > 0          
	,"AAH39 LOGIN ISSUE EXTRANET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	FISFLAG  > 0          
	,"AAH40 LOGIN ISSUE FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	FUSIONOPSFLAG  > 0          
	,"AAH41 LOGIN ISSUE FOPS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	INTERNETFLAG  > 0          
	,"AAH42 LOGIN ISSUE INTERNET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAH43 LOGIN ISSUE LAPTOP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	NETWORKFLAG  > 0          
	,"AAH44 LOGIN ISSUE NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0)           
	,"AAH45 LOGIN ISSUE OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	PHONEFLAG  > 0            
	,"AAH46 LOGIN ISSUE PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	PORTALFLAG  > 0            
	,"AAH47 LOGIN ISSUE PORTAL",        paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	REMOTEFLAG  > 0            
	,"AAH48 LOGIN ISSUE REMOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	SALESFORCEFLAG  > 0            
	,"AAH49 LOGIN ISSUE SFORCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	SFDCFLAG  > 0            
	,"AAH50 LOGIN ISSUE SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	TOMCATFLAG  > 0            
	,"AAH51 LOGIN ISSUE TOMCAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	TOOLFLAG  > 0           
	,"AAH52 LOGIN ISSUE TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0 & ETC$
	VPNFLAG  > 0           
	,"AAH53 LOGIN ISSUE VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGINFLAG  > 0                                        
	,"AAH99 LOGIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BLOCKFLAG  > 0 & 
	(ETC$VENDORFLAG  > 0| ETC$CUSTOMERFLAG  > 0)                                        
	,"AAI01 BLOCK VENDER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BLOCKFLAG  > 0 & 
	(ETC$INTERNETFLAG  > 0| ETC$SITEFLAG  > 0)                                        
	,"AAI02 BLOCK SITE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BLOCKFLAG  > 0                                        
	,"AAI99 BLOCK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & 
	(ETC$VENDORFLAG  > 0| ETC$CUSTOMERFLAG  > 0)                                        
	,"AAJ01 LOCK VENDER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	ADPFLAG  > 0           
	,"AAJ02 LOCK ADP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	CISCOFLAG  > 0          
	,"AAJ03 LOCK CISCO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAJ04 LOCK COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$OFFICEFLAG  > 0| ETC$EXCHANGEFLAG  > 0 )           
	,"AAJ05 LOCK OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	FISFLAG  > 0          
	,"AAJ06 LOCK FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	PHONEFLAG  > 0            
	,"AAJ07 LOCK PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	REMOTEFLAG  > 0            
	,"AAJ08 LOCK REMOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & ETC$
	VMFLAG  > 0           
	,"AAJ09 LOCK VM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAJ10 LOCK ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOCKFLAG  > 0                                        
	,"AAJ99 LOCK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$PLMFLAG  > 0)                                       
	,"AAK01 INSTALL PLM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$EPDMFLAG  > 0)                                       
	,"AAK02 INSTALL EPDM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$FISFLAG  > 0)                                       
	,"AAK03 INSTALL FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$VPNFLAG  > 0)                                       
	,"AAK04 INSTALL VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$CHINESEFLAG  > 0)                                       
	,"AAK05 INSTALL CHINESE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$ZIPFLAG  > 0)                                       
	,"AAK06 INSTALL ZIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$WINFLAG  > 0)                                       
	,"AAK07 INSTALL WINDOWS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$VISIOFLAG  > 0)                                       
	,"AAK08 INSTALL VISIO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$GOOGLEFLAG  > 0)                                       
	,"AAK09 INSTALL GOOGLE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0)                                       
	,"AAK10 INSTALL OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$OFFICEFLAG  > 0)                                       
	,"AAK11 INSTALL OFFICE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$PROJECTFLAG  > 0)                                       
	,"AAK12 INSTALL PROJECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$SCANFLAG  > 0)                                       
	,"AAK13 INSTALL SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$FAXFLAG  > 0)                                       
	,"AAK14 INSTALL FAX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"AAK15 INSTALL ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$CADFLAG  > 0)                                       
	,"AAK16 INSTALL CAD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$BWFLAG  > 0)                                       
	,"AAK17 INSTALL BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$ADOBEFLAG  > 0)                                       
	,"AAK18 INSTALL ADOBE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$ALTIUMFLAG  > 0)                                       
	,"AAK19 INSTALL ALTIUM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)                                       
	,"AAK20 INSTALL PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$CITRIXFLAG  > 0)                                       
	,"AAK21 INSTALL CITRIX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$DELLFLAG  > 0)                                       
	,"AAK22 INSTALL DELL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$DRIVEFLAG  > 0)                                       
	,"AAK23 INSTALL DRIVE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$ENGINEERFLAG  > 0)                                       
	,"AAK24 INSTALL ENGINEER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$EXCELFLAG  > 0)                                       
	,"AAK25 INSTALL EXCEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$PDFFLAG  > 0)                                       
	,"AAK26 INSTALL PDF",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	ETC$SKYPEFLAG  > 0                                       
	,"AAK27 INSTALL SKYPE",             paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$SQLFLAG  > 0)                                       
	,"AAK28 INSTALL SQL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$EXPENSFLAG  > 0)                                       
	,"AAK29 INSTALL EXPENS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$INTERNETFLAG  > 0| ETC$IEFLAG  > 0)                                       
	,"AAK30 INSTALL IE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$OPERATINGSYSTEMFLAG  > 0| ETC$OSFLAG  > 0)                                       
	,"AAK31 INSTALL OS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"AAK32 INSTALL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INSTALLFLAG  > 0 & 
	(ETC$SOFTWAREFLAG  > 0| ETC$APPFLAG  > 0)                                       
	,"AAK33 INSTALL SOFTWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INSTALLFLAG  > 0)                                       
	,"AAK99 INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$EPDMFLAG  > 0)                                       
	,"AAL01 SETUP EPDM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$VPNFLAG  > 0)                                       
	,"AAL02 SETUP VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$ZIPFLAG  > 0)                                       
	,"AAL03 SETUP ZIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$WINFLAG  > 0)                                       
	,"AAL04 SETUP WINDOWS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$EXCHANGEFLAG  > 0| ETC$EXCELFLAG  > 0)                                       
	,"AAL05 SETUP OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$PROJECTFLAG  > 0)                                       
	,"AAL06 SETUP PROJECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$SCANFLAG  > 0)                                       
	,"AAL07 SETUP SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$FAXFLAG  > 0)                                       
	,"AAL08 SETUP FAX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"AAL09 SETUP ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$ADOBEFLAG  > 0)                                       
	,"AAL10 SETUP ADOBE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$ALTIUMFLAG  > 0)                                       
	,"AAL11 SETUP ALTIUM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)                                       
	,"AAL12 SETUP PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$CISCOFLAG  > 0)                                       
	,"AAL13 SETUP CISCO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$CITRIXFLAG  > 0)                                       
	,"AAL14 SETUP CITRIX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$DELLFLAG  > 0)                                       
	,"AAL15 SETUP DELL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$DRIVEFLAG  > 0)                                       
	,"AAL16 SETUP DRIVE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$PDFFLAG  > 0)                                       
	,"AAL17 SETUP PDF",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$SKYPEFLAG  > 0)                                       
	,"AAL18 SETUP SKYPE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$EXPENSFLAG  > 0)                                       
	,"AAL19 SETUP EXPENS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$WIRELESSFLAG  > 0)                                       
	,"AAL20 SETUP WIRELESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$TABLETFLAG  > 0)                                       
	,"AAL21 SETUP TABLET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$STATIONFLAG  > 0)                                       
	,"AAL22 SETUP STATION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$TOOLFLAG  > 0)                                       
	,"AAL23 SETUP TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$NETWORKFLAG  > 0)                                       
	,"AAL24 SETUP NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$INTERNATIONALFLAG  > 0)                                       
	,"AAL25 SETUP INTERNATIONAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0| ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)                                       
	,"AAL26 SETUP TECHNICIAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$TS3FLAG  > 0)                                       
	,"AAL27 SETUP TS3",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$SFDCFLAG  > 0)                                       
	,"AAL28 SETUP SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$FUSIONOPSFLAG  > 0)                                       
	,"AAL29 SETUP FOPS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$SERVERFLAG  > 0)                                       
	,"AAL30 SETUP SERVRER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"AAL31 SETUP COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$INTERNETFLAG  > 0| ETC$IEFLAG  > 0)                                       
	,"AAL32 SETUP IE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$OPERATINGSYSTEMFLAG  > 0| ETC$OSFLAG  > 0)                                       
	,"AAL33 SETUP OS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"AAL34 SETUP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SETUPFLAG  > 0 & 
	(ETC$SOFTWAREFLAG  > 0| ETC$APPFLAG  > 0)                                       
	,"AAL35 SETUP SOFTWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SETUPFLAG  > 0)                                       
	,"AAL99 SETUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$OUTLOOKFLAG  > 0| ETC$EXCHANGEFLAG  > 0)                                       
	,"AAM01 CONFIG OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$OFFICEFLAG  > 0| ETC$EXCELFLAG  > 0)                                       
	,"AAM02 CONFIG OFFICE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"AAM03 CONFIG ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$PHONEFLAG  > 0)                                       
	,"AAM04 CONFIG PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	ETC$PROJECTFLAG  > 0                                      
	,"AAM05 CONFIG PROJECT",            paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$FISFLAG  > 0)                                       
	,"AAM06 CONFIG FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"AAM07 CONFIG COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"AAM08 CONFIG ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CONFIGFLAG  > 0                                      
	,"AAM99 CONFIG",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTERNETFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAN01 INTERNET ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTERNETFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0)          
	,"AAN02 INTERNET DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTERNETFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAN03 INTERNET ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTERNETFLAG  > 0                                        
	,"AAN99 INTERNET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VPNFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAO01 VPN ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VPNFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0)          
	,"AAO02 VPN DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VPNFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAO03 VPN ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VPNFLAG  > 0                                        
	,"AAO99 VPN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAP01 SERVER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$REBOOTFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAP02 SERVER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$PATCHFLAG  > 0)          
	,"AAP03 SERVER PATCH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$BACKUPFLAG  > 0)          
	,"AAP04 SERVER BACKUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$RESTORFLAG  > 0)          
	,"AAP05 SERVER RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAP06 SERVER VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$FILEFLAG  > 0)          
	,"AAP07 SERVER FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$ROLLFLAG  > 0)          
	,"AAP08 SERVER ROLLOUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAP09 SERVER ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"AAP10 SERVER ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$MAINTENANCEFLAG  > 0)          
	,"AAP11 SERVER MAINTENANCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAP12 SERVER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVERFLAG  > 0                                        
	,"AAP99 SERVER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$WIRELESSFLAG  > 0| ETC$REMOTEFLAG  > 0)          
	,"AAQ01 NETWORK WIRELESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAQ02 NETWORK ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAQ03 NETWORK DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$PATCHFLAG  > 0)          
	,"AAQ04 NETWORK PATCH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$BACKUPFLAG  > 0)          
	,"AAQ05 NETWORK BACKUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$RESTORFLAG  > 0)          
	,"AAQ06 NETWORK RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAQ07 NETWORK VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$FILEFLAG  > 0)          
	,"AAQ08 NETWORK FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$ROLLFLAG  > 0)          
	,"AAQ09 NETWORK ROLLOUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAQ10 NETWORK COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"AAQ11 NETWORK ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"AAQ12 NETWORK ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$MAINTENANCEFLAG  > 0)          
	,"AAQ13 NETWORK MAINTENANCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAQ14 NETWORK ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NETWORKFLAG  > 0                                        
	,"AAQ99 NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ROUTERFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAR01 ROUTER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ROUTERFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAR02 ROUTER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ROUTERFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAR03 ROUTER VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ROUTERFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAR04 ROUTER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ROUTERFLAG  > 0                                        
	,"AAR99 ROUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAS01 SWITCH ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAS02 SWITCH DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAS03 SWITCH VIRUS HACK",         paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAS04 SWITCH COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAS05 SWITCH ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SWITCHFLAG  > 0                                        
	,"AAS99 SWITCH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAT01 NODE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"AAT02 NODE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0 & 
	(ETC$REBOOTFLAG  > 0)          
	,"AAT03 NODE REBOOT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAT04 NODE VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAT05 NODE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NODEFLAG  > 0                                        
	,"AAT99 NODE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAU01 LINK ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"AAU02 LINK DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0 & 
	(ETC$REBOOTFLAG  > 0)          
	,"AAU03 LINK REBOOT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"AAU04 LINK VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAU05 LINK ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LINKFLAG  > 0                                        
	,"AAU99 LINK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAV01 STATION ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAV02 STATION DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0)          
	,"AAV03 STATION ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"AAV04 STATION ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAV05 STATION COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAV06 STATION ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$STATIONFLAG  > 0                                        
	,"AAV99 STATION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AAW01 VM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAW02 VM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0)          
	,"AAW03 VM ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"AAW04 VM ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)          
	,"AAW05 VM COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAW06 VM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VIRTUALMACHINEFLAG  > 0| ETC$VMFLAG  > 0)                                        
	,"AAW99 VM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EQUIPMENTFLAG  > 0 & 
	(ETC$FOLLOWFLAG  > 0)                                  
	,"AAX01 EQUIPMENT FOLLOWUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EQUIPMENTFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAX02 EQUIPMENT DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EQUIPMENTFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAX03 EQUIPMENT ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EQUIPMENTFLAG  > 0                                        
	,"AAX99 EQUIPMENT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INVENTORYFLAG  > 0                                        
	,"AAY99 INVENTORY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$JOBFLAG  > 0 & 
	(ETC$STEPFLAG  > 0)                                      
	,"AAZ01 JOB STEP1",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$JOBFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AAZ02 JOB DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$JOBFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AAZ03 JOB ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$JOBFLAG  > 0                                        
	,"AAZ99 JOB",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WINZIPFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABA02 WINZIP DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WINZIPFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABA03 WINZIP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WINZIPFLAG  > 0                                        
	,"ABA99 WINZIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WINDOWSFLAG  > 0                                        
	,"ABB99 WINDOWS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WINFLAG  > 0                                        
	,"ABC99 WIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PTOOLFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABD01 PTOOL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PTOOLFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABD02 PTOOL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PTOOLFLAG  > 0                                        
	,"ABD99 PTOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TOOLFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABE01 TOOL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TOOLFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABE02 TOOL ISSUE",                paste(ETC$KEYWORDVAR))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TOOLFLAG  > 0                                        
	,"ABE99 TOOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SFDCFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABF01 SFDC DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SFDCFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABF02 SFDC ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SFDCFLAG  > 0                                        
	,"ABF99 SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FISFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABG01 FIS ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FISFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABG02 FIS DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FISFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABG03 FIS ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FISFLAG  > 0                                        
	,"ABG99 FIS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EPDMFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABH01 EPDM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EPDMFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABH02 EPDM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EPDMFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABH03 EPDM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EPDMFLAG  > 0                                        
	,"ABH99 EPDM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PLMFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABI01 PLM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PLMFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABI02 PLM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PLMFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABI03 PLM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PLMFLAG  > 0                                        
	,"ABI99 PLM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TS3FLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABJ01 TS3 ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TS3FLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABJ02 TS3 DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TS3FLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABJ03 TS3 ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TS3FLAG  > 0                                        
	,"ABJ99 TS3",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REFUSOLFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABK01 REFUSOL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REFUSOLFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABK02 REFUSOL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REFUSOLFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABK03 REFUSOL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REFUSOLFLAG  > 0                                        
	,"ABK99 REFUSOL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ASTEAFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABL01 ASTEA ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ASTEAFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABL02 ASTEA DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ASTEAFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABL03 ASTEA ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ASTEAFLAG  > 0                                        
	,"ABL99 ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BWFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABM01 BW ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BWFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABM02 BW DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BWFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABM03 BW ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BWFLAG  > 0                                        
	,"ABM99 BW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BOMFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABN01 BOM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BOMFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABN02 BOM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BOMFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABN03 BOM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BOMFLAG  > 0                                        
	,"ABN99 BOM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FUSIONOPSFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABO01 FUSIONOPS ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FUSIONOPSFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABO02 FUSIONOPS DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FUSIONOPSFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABO03 FUSIONOPS ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FUSIONOPSFLAG  > 0                                        
	,"ABO99 FUSIONOPS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ALTIUMFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABP01 ALTIUM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ALTIUMFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABP02 ALTIUM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ALTIUMFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABP03 ALTIUM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ALTIUMFLAG  > 0                                        
	,"ABP99 ALTIUM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ENGINEERFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABQ01 ENGINEER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ENGINEERFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABQ02 ENGINEER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ENGINEERFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABQ03 ENGINEER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ENGINEERFLAG  > 0                                        
	,"ABQ99 ENGINEER",                  paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SHAREPOINTFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABR01 SHAREPOINT ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SHAREPOINTFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABR02 SHAREPOINT DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SHAREPOINTFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABR03 SHAREPOINT ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SHAREPOINTFLAG  > 0                                        
	,"ABR99 SHAREPOINT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PROJECTFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABS01 PROJECT ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PROJECTFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABS02 PROJECT DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PROJECTFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABS03 PROJECT ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PROJECTFLAG  > 0                                        
	,"ABS99 PROJECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PDFFLAG  > 0                                        
	,"ABT99 PDF",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ADOBEFLAG  > 0                                        
	,"ABU99 ADOBE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$GOOGLEFLAG  > 0                                        
	,"ABV99 GOOGLE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ATSFLAG  > 0                                        
	,"ABW99 ATS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ADPFLAG  > 0                                        
	,"ABX99 ADP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VISIOFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABY01 VISIO ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VISIOFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABY02 VISIO DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VISIOFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABY03 VISIO ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VISIOFLAG  > 0                                        
	,"ABY99 VISIO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SALESFORCEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ABZ01 SALESFORCE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SALESFORCEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ABZ02 SALESFORCE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SALESFORCEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ABZ03 SALESFORCE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SALESFORCEFLAG  > 0                                        
	,"ABZ99 SALESFORCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MSOFFICEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACA01 MS OFFICE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MSOFFICEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACA02 MS OFFICE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MSOFFICEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACA03 MS OFFICE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MSOFFICEFLAG  > 0                                        
	,"ACA99 MS OFFICE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OFFICEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACB01 OFFICE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OFFICEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACB02 OFFICE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OFFICEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACB03 OFFICE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OFFICEFLAG  > 0                                        
	,"ACB99 OFFICE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCELFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACC01 EXCEL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCELFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACC02 EXCEL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCELFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACC03 EXCEL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCELFLAG  > 0                                        
	,"ACC99 EXCEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OUTLOOKFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACD01 OUTLOOK ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OUTLOOKFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACD02 OUTLOOK DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OUTLOOKFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACD03 OUTLOOK ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OUTLOOKFLAG  > 0                                        
	,"ACD99 OUTLOOK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCHANGEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACE01 EXCHANGE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCHANGEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACE02 EXCHANGE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCHANGEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACE03 EXCHANGE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXCHANGEFLAG  > 0                                        
	,"ACE99 EXCHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTRANETFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACF01 INTRANET ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTRANETFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACF02 INTRANET DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTRANETFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACF03 INTRANET ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTRANETFLAG  > 0                                        
	,"ACF99 INTRANET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$EXTRANETFLAG  > 0                                        
	,"ACG99 EXTRANET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DELLFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACH01 DELL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DELLFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACH02 DELL DOWN NOTWORK",         paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DELLFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACH03 DELL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DELLFLAG  > 0                                        
	,"ACH99 DELL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CISCOFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACI01 CISCO ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CISCOFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACI02 CISCO DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CISCOFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACI03 CISCO ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CISCOFLAG  > 0                                        
	,"ACI99 CISCO",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CITRIXFLAG  > 0                                        
	,"ACJ99 CITRIX",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CADFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACK01 CAD ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CADFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACK02 CAD DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CADFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACK03 CAD ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CADFLAG  > 0                                        
	,"ACK99 CAD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SKYPEFLAG  > 0                                        
	,"ACL99 SKYPE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SQLFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACM01 SQL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SQLFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACM02 SQL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SQLFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACM03 SQL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SQLFLAG  > 0                                        
	,"ACM99 SQL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$KRONOSFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACN01 KRONOS ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$KRONOSFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACN02 KRONOS DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$KRONOSFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACN03 KRONOS ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$KRONOSFLAG  > 0                                        
	,"ACN99 KRONOS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBSITEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACO01 WEBSITE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBSITEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACO02 WEBSITE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBSITEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACO03 WEBSITE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBSITEFLAG  > 0                                        
	,"ACO99 WEBSITE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SITEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACP01 SITE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SITEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACP02 SITE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SITEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACP03 SITE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SITEFLAG  > 0                                        
	,"ACP99 SITE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MCAFEEFLAG  > 0                                        
	,"ACQ99 MCAFEE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SOLIDWORKFLAG  > 0                                        
	,"ACR99 SOLIDWORKS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TOMCATFLAG  > 0                                        
	,"ACS99 TOMCAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FOLDERFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACT01 FOLDER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FOLDERFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACT02 FOLDER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FOLDERFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACT03 FOLDER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FOLDERFLAG  > 0                                        
	,"ACT99 FOLDER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DRIVEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACU01 DRIVE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DRIVEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACU02 DRIVE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DRIVEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACU03 DRIVE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DRIVEFLAG  > 0                                        
	,"ACU99 DRIVE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FILEFLAG  > 0 &  
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACV01 FILE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FILEFLAG  > 0 &  
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACV02 FILE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FILEFLAG  > 0 &  
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACV03 FILE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FILEFLAG  > 0                                        
	,"ACV99 FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & ETC$AUTHORFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACW01 SYSTEM UNATHOR ACCESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACW02 SYSTEM ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACW03 SYSTEM DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$PATCHFLAG  > 0)          
	,"ACW04 SYSTEM PATCH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	ETC$BACKUPFLAG  > 0          
	,"ACW05 SYSTEM BACKUP",             paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$RESTORFLAG  > 0)          
	,"ACW06 SYSTEM RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"ACW07 SYSTEM VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$FILEFLAG  > 0)          
	,"ACW08 SYSTEM FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$ROLLFLAG  > 0)          
	,"ACW09 SYSTEM ROLLOUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"ACW10 SYSTEM ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"ACW11 SYSTEM ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$MAINTENANCEFLAG  > 0)          
	,"ACW12 SYSTEM MAINTENANCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACW13 SYSTEM ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SYSTEMFLAG  > 0                                        
	,"ACW99 SYSTEM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & ETC$AUTHORFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACX01 SERVICE UNATHOR ACCESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACX02 SERVICE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACX03 SERVICE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$PATCHFLAG  > 0)          
	,"ACX04 SERVICE PATCH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$BACKUPFLAG  > 0)          
	,"ACX05 SERVICE BACKUP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$RESTORFLAG  > 0)          
	,"ACX06 SERVICE RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"ACX07 SERVICE VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$FILEFLAG  > 0)          
	,"ACX08 SERVICE FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$ROLLFLAG  > 0)          
	,"ACX09 SERVICE ROLLOUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"ACX10 SERVICE ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"ACX11 SERVICE ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$MAINTENANCEFLAG  > 0)          
	,"ACX12 SERVICE MAINTENANCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACX13 SERVICE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SERVICEFLAG  > 0                                        
	,"ACX99 SERVICE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACY02 APP ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACY03 APP DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$PATCHFLAG  > 0)          
	,"ACY04 APP PATCH",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$BACKUPFLAG  > 0)          
	,"ACY05 APP BACKUP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$RESTORFLAG  > 0)          
	,"ACY06 APP RESTORE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$VIRUSFLAG  > 0| ETC$HACKFLAG  > 0)          
	,"ACY07 APP VIRUS HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$FILEFLAG  > 0)          
	,"ACY08 APP FILE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$ROLLFLAG  > 0)          
	,"ACY09 APP ROLLOUT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)          
	,"ACY10 APP ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0| ETC$RIGHTFLAG  > 0)          
	,"ACY11 APP ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$MAINTENANCEFLAG  > 0)          
	,"ACY12 APP MAINTENANCE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACY13 APP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$APPFLAG  > 0| ETC$APPLICATIONFLAG  > 0)                                       
	,"ACY99 APP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$JOBFLAG  > 0 & ETC$STEPFLAG  > 0
	,"ACZ01 NEW HIRE STEP1",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$EQUIPMENTFLAG  > 0
	,"ACZ02 NEW HIRE EQUIPMENT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & (ETC$WIRELESSFLAG  > 0| ETC$VPNFLAG  > 0)
	,"ACZ03 NEW HIRE WIRELESS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & (ETC$WIRELESSFLAG  > 0| ETC$VPNFLAG  > 0)
	,"ACZ04 NEW HIRE REMOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$HARDWAREFLAG  > 0
	,"ACZ05 NEW HIRE HARDWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$PHONEFLAG  > 0
	,"ACZ06 NEW HIRE PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$NETWORKFLAG  > 0
	,"ACZ07 NEW HIRE NETWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$REFUSOLFLAG  > 0
	,"ACZ08 NEW HIRE REFUSOL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$SFDCFLAG  > 0
	,"ACZ09 NEW HIRE SFDC",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$ASTEAFLAG  > 0
	,"ACZ10 NEW HIRE ASTEA",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & ETC$CODEFLAG  > 0
	,"ACZ11 NEW HIRE CODE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & (ETC$ACCOUNTFLAG  > 0| ETC$USERFLAG  > 0| ETC$ROLEFLAG  > 0| ETC$AGENTIDFLAG  > 0)
	,"ACZ12 NEW HIRE ACCOUNT",          paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0 & (ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)
	,"ACZ13 NEW HIRE COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & ETC$
	NEWFLAG  > 0
	,"ACZ14 NEW HIRE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TEAMFLAG  > 0| ETC$WEFLAG  > 0| ETC$USERSFLAG  > 0| ETC$EVERYBODYFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACZ15 USERS ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TEAMFLAG  > 0| ETC$WEFLAG  > 0| ETC$USERSFLAG  > 0| ETC$EVERYBODYFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACZ16 USERS DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TEAMFLAG  > 0| ETC$WEFLAG  > 0| ETC$USERSFLAG  > 0| ETC$EVERYBODYFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACZ17 USERS ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TEAMFLAG  > 0| ETC$WEFLAG  > 0| ETC$USERSFLAG  > 0| ETC$EVERYBODYFLAG  > 0)                                    
	,"ACZ18 USERS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ACZ19 USER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ACZ20 USER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ACZ21 USER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0)
	,"ACZ99 USER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$HARDWAREFLAG  > 0)                                      
	,"ADA01 NEW HARDWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$GENERALFLAG  > 0)                                      
	,"ADA02 NEW GENERAL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$ENGINEERFLAG  > 0)                                       
	,"ADA03 NEW ENGINEER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$SOFTWAREFLAG  > 0| ETC$APPFLAG  > 0)                                       
	,"ADA04 NEW SOFTWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$SECURITYFLAG  > 0)                                       
	,"ADA05 NEW SECURITY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$PHONEFLAG  > 0)                                       
	,"ADA06 NEW PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$SNCFLAG  > 0)                                       
	,"ADA06 NEW SNC",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"ADA07 NEW COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$NEWFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"ADA08 NEW ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$NEWFLAG  > 0
	,"ADA99 NEW",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$COMPANYFLAG  > 0)          
	,"ADB01 PHONE COMPANY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADB02 PHONE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0)          
	,"ADB03 PHONE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"ADB04 FIX PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"ADB05 PHONE COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"ADB06 PHONE ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"ADB07 PHONE INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$SWAPFLAG  > 0| ETC$LOANFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"ADB08 PHONE CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"ADB09 PHONE DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$INTERNFLAG  > 0)          
	,"ADB10 PHONE INTERNATIONAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$DIRECTFLAG  > 0| ETC$LISTFLAG  > 0)          
	,"ADB11 PHONE DIRECTORY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"ADB12 PHONE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PHONEFLAG  > 0                                        
	,"ADB99 PHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADC01 COPIER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	DELAYFLAG  > 0| ETC$HALFFLAG  > 0| ETC$SIDEWAYSFLAG  > 0| ETC$JAMFLAG  > 0| ETC$ALERTFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$
	CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$NOTAVAILABLEFLAG  > 0)          
	,"ADC02 COPIER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"ADC03 FIX COPIER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"ADC04 COPIER COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"ADC05 COPIER ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"ADC06 COPIER INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$SWAPFLAG  > 0| ETC$LOANFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"ADC07 COPIER CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"ADC08 COPIER DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"ADC09 COPIER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COPIERFLAG  > 0| ETC$COPYMACHINEFLAG  > 0| ETC$COPYFLAG  > 0)                                        
	,"ADC99 COPIER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADD01 FAX ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	DELAYFLAG  > 0| ETC$HALFFLAG  > 0| ETC$SIDEWAYSFLAG  > 0| ETC$JAMFLAG  > 0| ETC$ALERTFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$
	CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$NOTAVAILABLEFLAG  > 0)          
	,"ADD02 FAX DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)          
	,"ADD03 FIX FAX",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"ADD04 FAX COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FAXFLAG  > 0 & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"ADD05 FAX ADMIN",                 paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"ADD06 FAX INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$SWAPFLAG  > 0| ETC$LOANFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"ADD07 FAX CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"ADD08 FAX DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)                                       
	,"ADD09 FAX ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FAXFLAG  > 0)                                        
	,"ADD99 FAX",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADE01 SCAN ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$WORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	DELAYFLAG  > 0| ETC$HALFFLAG  > 0| ETC$SIDEWAYSFLAG  > 0| ETC$JAMFLAG  > 0| ETC$ALERTFLAG  > 0| ETC$UNABLEFLAG  > 0| ETC$NOTABLEFLAG  > 0| ETC$
	CANNOTFLAG  > 0| ETC$CANTFLAG  > 0| ETC$NOTAVAILABLEFLAG  > 0)          
	,"ADE02 SCAN DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$NOTCHARGFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$CALIBRATFLAG  > 0| ETC$CHARGFLAG  > 0| ETC$DEADFLAG  > 0)          
	,"ADE03 FIX SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$COMPUTERFLAG  > 0| ETC$LAPTOPFLAG  > 0| ETC$MACHINEFLAG  > 0| ETC$PCFLAG  > 0| ETC$DESKTOPFLAG  > 0)                                       
	,"ADE04 SCAN COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)                                       
	,"ADE05 SCAN ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$CONFIGFLAG  > 0)          
	,"ADE06 SCAN INSTALL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$SWAPFLAG  > 0| ETC$LOANFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)          
	,"ADE07 SCAN CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"ADE08 SCAN DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$RECEIVFLAG  > 0| ETC$OPENFLAG  > 0| ETC$SENDFLAG  > 0| ETC$DELIVERFLAG  > 0| ETC$TRANSFERFLAG  > 0| ETC$FORWARDFLAG  > 0)          
	,"ADE09 SCAN DELIVERY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0) & 
	(ETC$INCIDENTFLAG  > 0)                                       
	,"ADE10 SCAN ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SCANFLAG  > 0)                                        
	,"ADE99 SCAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INCIDENTFLAG  > 0 & ETC$
	ATTACKFLAG  > 0                                        
	,"ADF01 INCIDENT ATTACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INCIDENTFLAG  > 0                                        
	,"ADF99 INCIDENT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CRITICALFLAG  > 0 & ETC$
	ALERTFLAG  > 0                                        
	,"ADG01 CRITICAL ALERT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CRITICALFLAG  > 0                                        
	,"ADG99 CRITICAL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REQFLAG  > 0 & ETC$
	DEBUGFLAG  > 0                                        
	,"ADH01 DEBUG REQ",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REQFLAG  > 0                                        
	,"ADH99 REQ",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SNCFLAG  > 0                                        
	,"ADI99 SNC",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SQFLAG  > 0                                        
	,"ADJ99 SQ",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ACROBATFLAG  > 0                                        
	,"ADK99 ACROBAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ITCHECKFLAG  > 0                                        
	,"ADL99 ITCHECK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FLASHFLAG  > 0                                        
	,"ADM99 FLASH",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CPUFLAG  > 0                                        
	,"ADN99 CPU",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$VOIPFLAG  > 0                                        
	,"ADO99 VOIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$USBFLAG  > 0                                        
	,"ADP99 USB",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$REPORTFLAG  > 0)                                    
	,"ADQ99 REPORT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SOFTWAREFLAG  > 0                                        
	,"ADR99 SOFTWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$HARDWAREFLAG  > 0                                        
	,"ADS99 HARDWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INTERFACEFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADT01 INTERFACE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INTERFACEFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADT02 INTERFACE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INTERFACEFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADT03 INTERFACE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INTERFACEFLAG  > 0                                        
	,"ADT99 INTERFACE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$QUEUEFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADU01 QUEUE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$QUEUEFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADU02 QUEUE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$QUEUEFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADU03 QUEUE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$QUEUEFLAG  > 0)                                        
	,"ADU99 QUEUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTALFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADV01 PORTAL ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTALFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADV02 PORTAL DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTALFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADV03 PORTAL ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PORTALFLAG  > 0                                        
	,"ADV99 PORTAL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DOMAINFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADW01 DOMAIN ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DOMAINFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADW02 DOMAIN DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DOMAINFLAG  > 0 & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADW03 DOMAIN ISSUE",              paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))

ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DOMAINFLAG  > 0                                        
	,"ADW99 DOMAIN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIRELESSFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADX01 WIRELESS ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIRELESSFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADX02 WIRELESS DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIRELESSFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADX03 WIRELESS ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIRELESSFLAG)  > 0                                        
	,"ADX99 WIRELESS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIFIFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADY01 WIFI ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIFIFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADY02 WIFI DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WIFIFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADY03 WIFI ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WIFIFLAG  > 0                                        
	,"ADY99 WIFI",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$REMOTEFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"ADZ01 REMOTE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$REMOTEFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"ADZ02 REMOTE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$REMOTEFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"ADZ03 REMOTE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REMOTEFLAG  > 0                                        
	,"ADZ99 REMOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LANFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AEA01 LAN ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LANFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AEA02 LAN DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LANFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AEA03 LAN ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LANFLAG  > 0                                        
	,"AEA99 LAN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$BOXFLAG  > 0                                        
	,"AEB99 BOX",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$IPADFLAG  > 0| ETC$IPHONEFLAG  > 0)                                        
	,"AEC99 IPAD IPHONE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MOVEFLAG  > 0) & 
	(ETC$USERFLAG  > 0| ETC$NEWFLAG  > 0| ETC$DESKFLAG  > 0)                                      
	,"AEF01 MOVE DESK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MOVEFLAG  > 0)                                    
	,"AEF99 MOVE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$KEYBOARDFLAG  > 0)                                    
	,"AEG99 KEYBOARD",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MONITORFLAG  > 0)                                    
	,"AEH99 MONITOR",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MOUSEFLAG  > 0)                                    
	,"AEI99 MOUSE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$HEADSETFLAG  > 0)                                    
	,"AEJ99 HEADSET",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$RAMFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CONFIGFLAG  > 0| ETC$REPLACEFLAG  > 0)                                   
	,"AEK01 RAM NEW",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$RAMFLAG  > 0)                                    
	,"AEL99 RAM",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MEMORYFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CONFIGFLAG  > 0| ETC$REPLACEFLAG  > 0)                                   
	,"AEM01 MEMORY NEW",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MEMORYFLAG  > 0)                                    
	,"AEM99 MEMORY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SPACEFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CONFIGFLAG  > 0| ETC$REPLACEFLAG  > 0)                                   
	,"AEN01 SPACE NEW",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SPACEFLAG  > 0)                                    
	,"AEN99 SPACE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$VOLUMEFLAG  > 0) & 
	(ETC$ADDFLAG  > 0| ETC$INSTALLFLAG  > 0| ETC$SETUPFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CONFIGFLAG  > 0| ETC$REPLACEFLAG  > 0)                                   
	,"AEO01 VOLUME NEW",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$VOLUMEFLAG  > 0)                                    
	,"AEO99 VOLUME",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PAYROLLFLAG  > 0)                                    
	,"AEP99 PAYROLL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PTOFLAG  > 0)                                    
	,"AEQ99 PTO",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DASHBOARDFLAG  > 0)                                    
	,"AER99 DASHBOARD",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WGFLAG  > 0)                                    
	,"AES99 WG",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$EXPENSFLAG  > 0)                                    
	,"AET99 EXPENSE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AEU01 PORT ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AEU02 PORT DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PORTFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AEU03 PORT ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PORTFLAG  > 0                                        
	,"AEU99 PORT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MATERIALFLAG  > 0)                                    
	,"AEV99 MATERIAL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PARTFLAG  > 0)                                    
	,"AEW99 PART",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SHIPFLAG  > 0)                                    
	,"AEX99 SHIP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ORDERFLAG  > 0)                                    
	,"AEY99| ETC$DER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$WARRANTYFLAG  > 0)                                    
	,"AEZ99 WARRANTY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INVOICEFLAG  > 0)                                    
	,"AFA99 INVOICE",                   paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PURCHFLAG  > 0)                                    
	,"AFB99 PURCHASE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$QUOTEFLAG  > 0)                                    
	,"AFC99 QUOTE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SALEFLAG  > 0)                                    
	,"AFD99 SALE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TAXFLAG  > 0)                                    
	,"AFE99 TAX",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$BILLFLAG  > 0)                                    
	,"AFF99 BILL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DELIVERFLAG  > 0)                                    
	,"AFG99 DELIVER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FIREWALLFLAG  > 0)                                    
	,"AFH99 FIREWALL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$BARCODEFLAG  > 0)                                    
	,"AFI99 BARCODE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$CODEFLAG  > 0)                                    
	,"AFJ99 CODE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$GROUPFLAG  > 0)                                    
	,"AFK99 GROUP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$CASEFLAG  > 0)                                    
	,"AFL99 CASE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LISTFLAG  > 0)                                    
	,"AFM99 LIST",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ROLEFLAG  > 0)                                    
	,"AFN99 ROLE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$BACKUPFLAG  > 0)                                    
	,"AFO99 BACKUP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$FTPFLAG  > 0)                                    
	,"AFP99 FTP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$EFTFLAG  > 0)                                    
	,"AFQ99 EFT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TONERFLAG  > 0)                                    
	,"AFR99 TONER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INKFLAG  > 0)                                    
	,"AFS99 INK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DOCFLAG  > 0)                                    
	,"AFT99 DOC",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$INFOFLAG  > 0)                                    
	,"AFU99 INFO",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MEETFLAG  > 0)                                    
	,"AFV99 MEET",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$CONFERENCEFLAG  > 0)                                    
	,"AFW99 CONFERENCE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$CALLFLAG  > 0)                                    
	,"AFX99 CALL",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$CALENDARFLAG  > 0)                                    
	,"AFY99 CALENDAR",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ADDRESSFLAG  > 0)                                    
	,"AFZ99 ADDRESS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$BATTERYFLAG  > 0)                                    
	,"AGA99 BATTERY",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$POWERFLAG  > 0)                                    
	,"AGB99 POWER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MACHINEFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGC01 MACHINE ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MACHINEFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGC02 MACHINE DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MACHINEFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGC03 MACHINE ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MACHINEFLAG  > 0)                                    
	,"AGC99 MACHINE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COMPUTERFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGD01 COMPUTER ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COMPUTERFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGD02 COMPUTER DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COMPUTERFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGD03 COMPUTER ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COMPUTERFLAG  > 0)                                    
	,"AGD99 COMPUTER",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LAPTOPFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGE01 LAPTOP ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LAPTOPFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGE02 LAPTOP DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LAPTOPFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGE03 LAPTOP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$LAPTOPFLAG  > 0)                                    
	,"AGE99 LAPTOP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DESKTOPFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGF01 DESKTOP ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DESKTOPFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGF02 DESKTOP DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DESKTOPFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGF03 DESKTOP ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DESKTOPFLAG  > 0)                                    
	,"AGF99 DESKTOP",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PCFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGG01 PC ACESS CONNECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PCFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGG02 PC DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PCFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGG03 PC ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PCFLAG  > 0)                                    
	,"AGG99 PC",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TIMEFLAG  > 0| ETC$CLOCKFLAG  > 0) & 
	(ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)          
	,"AGH01 TIME ACESS CONNECT",        paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TIMEFLAG  > 0| ETC$CLOCKFLAG  > 0) & 
	(ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0| ETC$
	ALERTFLAG  > 0| ETC$RESETFLAG  > 0| ETC$RESTARTFLAG  > 0)          
	,"AGH02 TIME DOWN NOTWORK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TIMEFLAG  > 0| ETC$CLOCKFLAG  > 0) & 
	(ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)          
	,"AGH03 TIME ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$TIMEFLAG  > 0| ETC$CLOCKFLAG  > 0)                                    
	,"AGH99 TIME",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$HACKFLAG  > 0)                                    
	,"AGI99 HACK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ANONYMOUSFLAG  > 0)                                    
	,"AGJ99 ANONYMOUS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$VIRUSFLAG  > 0| ETC$INFECTFLAG  > 0)                                    
	,"AGK99 VIRUS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$MALWAREFLAG  > 0)                                    
	,"AGL99 MALWARE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$SPAMFLAG  > 0)                                    
	,"AGM99 SPAM",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$DETECTFLAG  > 0)                                    
	,"AGN99 DETECT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$HUNGFLAG  > 0| ETC$CRASHFLAG  > 0| ETC$FREEZEFLAG  > 0| ETC$NOTWORKFLAG  > 0| ETC$DOESNOTWORKFLAG  > 0| ETC$OFFLINEFLAG  > 0| ETC$
	DOESNTWORKFLAG  > 0| ETC$ISNTWORKFLAG  > 0| ETC$QUITWORKFLAG  > 0| ETC$WONTWORKFLAG  > 0| ETC$STOPPEDWORKFLAG  > 0| ETC$
	SLOWFLAG  > 0| ETC$DOWNFLAG  > 0| ETC$KICKEDOUTFLAG  > 0| ETC$NOTSTARTFLAG  > 0| ETC$OPENFLAG  > 0| ETC$FAILFLAG  > 0| ETC$OUTAGEFLAG  > 0| ETC$
	REBOOTFLAG  > 0| ETC$BROKEFLAG  > 0| ETC$BADFLAG  > 0| ETC$REPLACEFLAG  > 0| ETC$FIXFLAG  > 0| ETC$REPAIRFLAG  > 0)                                    
	,"AGO99 DOWN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$COMMUNICATFLAG  > 0| ETC$ACCESSFLAG  > 0| ETC$CONNECTFLAG  > 0| ETC$RESPONDFLAG  > 0| ETC$FUNCTIONFLAG  > 0)
	,"AGP99 ACCESS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ADMINFLAG  > 0| ETC$AUTHORFLAG  > 0| ETC$PERMISSIONFLAG  > 0| ETC$PRIVILFLAG  > 0)
	,"AGQ99 ADMIN",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$USERFLAG  > 0| ETC$HIREFLAG  > 0| ETC$EMPLOYEEFLAG  > 0| ETC$INTERNFLAG  > 0| ETC$POSITIONFLAG  > 0| ETC$ACCOUNTFLAG  > 0| ETC$TECHFLAG  > 0| ETC$CONTRACTFLAG  > 0)
	,"AGR99 ACCOUNT",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ADDFLAG  > 0| ETC$NEWFLAG  > 0| ETC$CHANGEFLAG  > 0| ETC$SWAPFLAG  > 0| ETC$LOANFLAG  > 0| ETC$CREATFLAG  > 0| ETC$REMOVFLAG  > 0| ETC$UPDATEFLAG  > 0| ETC$UPGRADEFLAG  > 0| ETC$IMPLEMENTFLAG  > 0| ETC$EXTENDFLAG  > 0| ETC$APPROVFLAG  > 0| ETC$DELETFLAG  > 0)                                    
	,"AGS99 CHANGE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$PROBLEMFLAG  > 0| ETC$ISSUEFLAG  > 0| ETC$ERRORFLAG  > 0| ETC$TROUBLEFLAG  > 0)
	,"AGT99 ISSUE",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$HELPDESKFLAG  > 0)                                    
	,"AGU99 HELP DESK",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$ITSUPPORTFLAG  > 0)                                    
	,"AGV99 ITSUPPORT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$QUICKFLAG  > 0                                        
	,"AGW99 QUICKEN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ENERGYFLAG  > 0                                        
	,"AGX99 ENERGY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DONGLEFLAG  > 0                                        
	,"AGY99 DONGLE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DATABASEFLAG  > 0                                        
	,"AGZ99 DATABASE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DATAFLAG  > 0                                        
	,"AHA99 DATA",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DEBUGFLAG  > 0                                        
	,"AHB99 DEBUG",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ASSETFLAG  > 0                                        
	,"AHC99 ASSET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ALERTFLAG  > 0                                        
	,"AHD99 ALERT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CHARTFLAG  > 0                                        
	,"AHE99 CHART",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$DIRECTFLAG  > 0                                        
	,"AHF99 DIRECTORY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FIRMFLAG  > 0                                        
	,"AHG99 FIRM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$FORMATFLAG  > 0                                        
	,"AHH99 FORMAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$GEARFLAG  > 0                                        
	,"AHI99 GEAR",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$INVERTFLAG  > 0                                        
	,"AHJ99 INVERT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$ITEMFLAG  > 0                                        
	,"AHK99 ITEM",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LABELFLAG  > 0                                        
	,"AHL99 LABEL",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LICENSFLAG  > 0                                        
	,"AHM99 LICENSE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MESSAGEFLAG  > 0                                        
	,"AHN99 MESSAGE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$OUTPUTFLAG  > 0                                        
	,"AHO99 OUTPUT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PACKFLAG  > 0                                        
	,"AHP99 PACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PROCESSFLAG  > 0                                        
	,"AHQ99 PROCESS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRODUCTIONFLAG  > 0                                        
	,"AHR99 PRODUCTION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PRODUCTFLAG  > 0                                        
	,"AHS99 PRODUCT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$RECORDFLAG  > 0                                        
	,"AHT99 RECORD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$RECRUITFLAG  > 0                                        
	,"AHU99 RECRUIT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$RESETFLAG  > 0                                        
	,"AHV99 RESET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$RESTARTFLAG  > 0                                        
	,"AHW99 RESTART",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$RESOURCEFLAG  > 0                                        
	,"AHX99 RESOURCE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SCREENFLAG  > 0                                        
	,"AHY99 SCREEN",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SLIPFLAG  > 0                                        
	,"AHZ99 SLIP",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$SPYWAREFLAG  > 0                                        
	,"AIA99 SPYWARE",                   paste(ETC$KEYWORDVAR)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR <-	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TERMINATIONFLAG  > 0                                        
	,"AIB99 TERMINATION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TRANSACTIONFLAG  > 0                                        
	,"AIC99 TRANSACTION",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TRACKFLAG  > 0                                        
	,"AID99 TRACK",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$UPLOADFLAG  > 0                                        
	,"AIE99 UPLOAD",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$UPSFLAG  > 0                                        
	,"AIF99 UPS",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$WEBFLAG  > 0                                        
	,"AIG99 WEB",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TICKETFLAG  > 0                                        
	,"AIH99 TICKET",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$REFUFLAG  > 0                                        
	,"AII99 REFU",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$PAYFLAG  > 0                                        
	,"AIJ99 PAY",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$LOGFLAG  > 0                                        
	,"AIK99 LOG",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MATFLAG  > 0                                        
	,"AIL99 MAT",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CARTFLAG  > 0                                        
	,"AIM99 CART",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$CARFLAG  > 0                                        
	,"AIN99 CAR",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$OPERATINGSYSTEMFLAG  > 0| ETC$OSFLAG  > 0)
	,"AIO99 OS",
	ifelse (ETC$KEYWORDVAR=="NA" & (ETC$IEFLAG  > 0)
	,"AIP99 IE",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TRAININGFLAG  > 0                                        
	,"AIQ99 TRAINING",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$TESTFLAG  > 0                                        
	,"AIR99 TEST",
	ifelse (ETC$KEYWORDVAR=="NA" & ETC$MAINTENANCEFLAG  > 0                                        
	,"AIS99 MAINTENANCE",               paste(ETC$KEYWORDVAR)))))))))))))))))))


ETC$KEYWORDVAR <- ifelse (ETC$KEYWORDVAR == "NA", "Other", paste(ETC$KEYWORDVAR))



###################################################################################################



ETC$KEYWORDVAR_O1 <- "NA"

ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAA03 PASSWORD RESET EXCHANGE",1,	
	ifelse (ETC$KEYWORDVAR =="AAA05 PASSWORD RESET NETWORK",2,	
	ifelse (ETC$KEYWORDVAR =="AAA09 PASSWORD RESET WEBEX",3,	
	ifelse (ETC$KEYWORDVAR =="AAA12 PASSWORD COMPUTER",4,	
	ifelse (ETC$KEYWORDVAR =="AAA20 PASSWORD ACCOUNT",5,	
	ifelse (ETC$KEYWORDVAR =="AAA24 PASSWORD USER",6,	
	ifelse (ETC$KEYWORDVAR =="AAA25 PASSWORD RESET ADP",7,	
	ifelse (ETC$KEYWORDVAR =="AAA27 PASSWORD RESET VPN",8,	
	ifelse (ETC$KEYWORDVAR =="AAA29 PASSWORD RESET ASTEA",9,	
	ifelse (ETC$KEYWORDVAR =="AAA31 PASSWORD RESET SFDC",10,	
	ifelse (ETC$KEYWORDVAR =="AAA33 PASSWORD RESET ITCHECK",11,	
	ifelse (ETC$KEYWORDVAR =="AAA34 PASSWORD ITCHECK",12,	
	ifelse (ETC$KEYWORDVAR =="AAA35 PASSWORD RESET PCARD",13,	
	ifelse (ETC$KEYWORDVAR =="AAA40 PASSWORD TS3",14,	
	ifelse (ETC$KEYWORDVAR =="AAA42 PASSWORD FIS",15,	
	ifelse (ETC$KEYWORDVAR =="AAA43 PASSWORD RESET BW",16,	
	ifelse (ETC$KEYWORDVAR =="AAA44 PASSWORD BW",17,	
	ifelse (ETC$KEYWORDVAR =="AAA45 PASSWORD RESET AE",18,	
	ifelse (ETC$KEYWORDVAR =="AAA46 PASSWORD AE",19,	
	ifelse (ETC$KEYWORDVAR =="AAB05 SAP SERVER",20,	
	ifelse (ETC$KEYWORDVAR =="AAB18 SAP RESET",21,	
	ifelse (ETC$KEYWORDVAR =="AAB38 SAP TOOL",22,	
	ifelse (ETC$KEYWORDVAR =="AAC01 WEBEX DISABLE",23,	
	ifelse (ETC$KEYWORDVAR =="AAC09 WEBEX AUTH",24,	
	ifelse (ETC$KEYWORDVAR =="AAC11 WEBEX OUTLOOK",25,	
	ifelse (ETC$KEYWORDVAR =="AAC16 WEBEX REQUEST",26,	
	ifelse (ETC$KEYWORDVAR =="AAD02 VOICEMAIL ACESS CONNECT",27,	
	ifelse (ETC$KEYWORDVAR =="AAD04 VOICEMAIL LOGIN",28,	
	ifelse (ETC$KEYWORDVAR =="AAD06 VOICEMAIL CHANGE",29,	
	ifelse (ETC$KEYWORDVAR =="AAE13 EMAIL RESTORE",30,	
	ifelse (ETC$KEYWORDVAR =="AAF06 MAIL SERVER",31,	
	ifelse (ETC$KEYWORDVAR =="AAF09 MAIL SCAN",32,	
	ifelse (ETC$KEYWORDVAR =="AAG06 PRINT TONER",33,	
	ifelse (ETC$KEYWORDVAR =="AAG14 PRINT JOB",34,	
	ifelse (ETC$KEYWORDVAR =="AAH02 LOGIN NOT WORK CISCO",35,	
	ifelse (ETC$KEYWORDVAR =="AAH05 LOGIN NOT WORK DOMAIN",36,	
	ifelse (ETC$KEYWORDVAR =="AAH07 LOGIN NOT WORK EPMD",37,	
	ifelse (ETC$KEYWORDVAR =="AAH08 LOGIN NOT WORK EXCHANGE",38,	
	ifelse (ETC$KEYWORDVAR =="AAH15 LOGIN NOT WORK OUTLOOK",39,	
	ifelse (ETC$KEYWORDVAR =="AAH17 LOGIN NOT WORK PORTAL",40,	
	ifelse (ETC$KEYWORDVAR =="AAH20 LOGIN NOT WORK SFDC",41,	
	ifelse (ETC$KEYWORDVAR =="AAH25 LOGIN ACCESS CONNECT",42,	
	ifelse (ETC$KEYWORDVAR =="AAH27 NEW LOGIN",43,	
	ifelse (ETC$KEYWORDVAR =="AAH34 LOGIN ISSUE DELL",44,	
	ifelse (ETC$KEYWORDVAR =="AAH36 LOGIN ISSUE EMPLOYEE",45,	
	ifelse (ETC$KEYWORDVAR =="AAH43 LOGIN ISSUE LAPTOP",46,	
	ifelse (ETC$KEYWORDVAR =="AAH49 LOGIN ISSUE SFORCE",47,	
	ifelse (ETC$KEYWORDVAR =="AAH50 LOGIN ISSUE SFDC",48,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAH52 LOGIN ISSUE TOOL",49,	
	ifelse (ETC$KEYWORDVAR =="AAH53 LOGIN ISSUE VPN",50,	
	ifelse (ETC$KEYWORDVAR =="AAJ03 LOCK CISCO",51,	
	ifelse (ETC$KEYWORDVAR =="AAJ06 LOCK FIS",52,	
	ifelse (ETC$KEYWORDVAR =="AAK06 INSTALL ZIP",53,	
	ifelse (ETC$KEYWORDVAR =="AAK14 INSTALL FAX",54,	
	ifelse (ETC$KEYWORDVAR =="AAK17 INSTALL BW",55,	
	ifelse (ETC$KEYWORDVAR =="AAK18 INSTALL ADOBE",56,	
	ifelse (ETC$KEYWORDVAR =="AAK21 INSTALL CITRIX",57,	
	ifelse (ETC$KEYWORDVAR =="AAK25 INSTALL EXCEL",58,	
	ifelse (ETC$KEYWORDVAR =="AAK27 INSTALL SKYPE",59,	
	ifelse (ETC$KEYWORDVAR =="AAL06 SETUP PROJECT",60,	
	ifelse (ETC$KEYWORDVAR =="AAL07 SETUP SCAN",61,	
	ifelse (ETC$KEYWORDVAR =="AAL13 SETUP CISCO",62,	
	ifelse (ETC$KEYWORDVAR =="AAL18 SETUP SKYPE",63,	
	ifelse (ETC$KEYWORDVAR =="AAL19 SETUP EXPENS",64,	
	ifelse (ETC$KEYWORDVAR =="AAL21 SETUP TABLET",65,	
	ifelse (ETC$KEYWORDVAR =="AAL35 SETUP SOFTWARE",66,	
	ifelse (ETC$KEYWORDVAR =="AAM03 CONFIG ADMIN",67,	
	ifelse (ETC$KEYWORDVAR =="AAP03 SERVER PATCH",68,	
	ifelse (ETC$KEYWORDVAR =="AAP11 SERVER MAINTENANCE",69,	
	ifelse (ETC$KEYWORDVAR =="AAQ05 NETWORK BACKUP",70,	
	ifelse (ETC$KEYWORDVAR =="AAQ08 NETWORK FILE",71,	
	ifelse (ETC$KEYWORDVAR =="AAR99 ROUTER",72,	
	ifelse (ETC$KEYWORDVAR =="AAV04 STATION ADMIN",73,	
	ifelse (ETC$KEYWORDVAR =="ABA02 WINZIP DOWN NOTWORK",74,	
	ifelse (ETC$KEYWORDVAR =="ABA99 WINZIP",75,	
	ifelse (ETC$KEYWORDVAR =="ABJ02 TS3 DOWN NOTWORK",76,	
	ifelse (ETC$KEYWORDVAR =="ABK02 REFUSOL DOWN NOTWORK",77,	
	ifelse (ETC$KEYWORDVAR =="ABY01 VISIO ACESS CONNECT",78,	
	ifelse (ETC$KEYWORDVAR =="ABZ03 SALESFORCE ISSUE",79,	
	ifelse (ETC$KEYWORDVAR =="ACA02 MS OFFICE DOWN NOTWORK",80,	
	ifelse (ETC$KEYWORDVAR =="ACF03 INTRANET ISSUE",81,	
	ifelse (ETC$KEYWORDVAR =="ACK01 CAD ACESS CONNECT",82,	
	ifelse (ETC$KEYWORDVAR =="ACV03 FILE ISSUE",83,	
	ifelse (ETC$KEYWORDVAR =="ACW07 SYSTEM VIRUS HACK",84,	
	ifelse (ETC$KEYWORDVAR =="ACW11 SYSTEM ADMIN",85,	
	ifelse (ETC$KEYWORDVAR =="ACY09 APP ROLLOUT",86,	
	ifelse (ETC$KEYWORDVAR =="ACZ06 NEW HIRE PHONE",87,	
	ifelse (ETC$KEYWORDVAR =="ACZ13 NEW HIRE COMPUTER",88,	
	ifelse (ETC$KEYWORDVAR =="ADB05 PHONE COMPUTER",89,	
	ifelse (ETC$KEYWORDVAR =="ADB06 PHONE ADMIN",90,	
	ifelse (ETC$KEYWORDVAR =="ADC01 COPIER ACESS CONNECT",91,	
	ifelse (ETC$KEYWORDVAR =="ADD04 FAX COMPUTER",92,	
	ifelse (ETC$KEYWORDVAR =="ADE01 SCAN ACESS CONNECT",93,	
	ifelse (ETC$KEYWORDVAR =="ADK99 ACROBAT",94,	
	ifelse (ETC$KEYWORDVAR =="ADU99 QUEUE",95,	
	ifelse (ETC$KEYWORDVAR =="ADY02 WIFI DOWN NOTWORK",96,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ADY03 WIFI ISSUE",97,	
	ifelse (ETC$KEYWORDVAR =="ADY99 WIFI",98,	
	ifelse (ETC$KEYWORDVAR =="AEP99 PAYROLL",99,	
	ifelse (ETC$KEYWORDVAR =="AGH02 TIME DOWN NOTWORK",100,	
	ifelse (ETC$KEYWORDVAR =="AGM99 SPAM",101,	
	ifelse (ETC$KEYWORDVAR =="AHE99 CHART",102,	
	ifelse (ETC$KEYWORDVAR =="AHW99 RESTART",103,	
	ifelse (ETC$KEYWORDVAR =="AHX99 RESOURCE",104,	
	ifelse (ETC$KEYWORDVAR =="AIB99 TERMINATION",105,	
	ifelse (ETC$KEYWORDVAR =="AIS99 MAINTENANCE",106,	
	ifelse (ETC$KEYWORDVAR =="AAA47 PASSWORD RESET AUTO REPLY",107,	
	ifelse (ETC$KEYWORDVAR =="AAD01 VOICEMAIL DISABLE",108,	
	ifelse (ETC$KEYWORDVAR =="AAJ10 LOCK ACCOUNT",109,	
	ifelse (ETC$KEYWORDVAR =="AAA08 PASSWORD SAP",110,	
	ifelse (ETC$KEYWORDVAR =="AAA49 PASSWORD RESET",111,	
	ifelse (ETC$KEYWORDVAR =="AAK05 INSTALL CHINESE",112,	
	ifelse (ETC$KEYWORDVAR =="AAA07 PASSWORD RESET SAP",113,	
	ifelse (ETC$KEYWORDVAR =="AAC06 WEBEX ACCOUNT",114,	
	ifelse (ETC$KEYWORDVAR =="AAF01 MAIL DELIVERY",115,	
	ifelse (ETC$KEYWORDVAR =="AAC02 WEBEX ACESS",116,	
	ifelse (ETC$KEYWORDVAR =="AAA11 PASSWORD RESET COMPUTER",117,	
	ifelse (ETC$KEYWORDVAR =="AAE16 EMAIL GROUP",118,	
	ifelse (ETC$KEYWORDVAR =="AAJ07 LOCK PHONE",119,	
	ifelse (ETC$KEYWORDVAR =="ADB01 PHONE COMPANY",120,	
	ifelse (ETC$KEYWORDVAR =="AAA02 PASSWORD EMAIL",121,	
	ifelse (ETC$KEYWORDVAR =="AAA10 PASSWORD WEBEX",122,	
	ifelse (ETC$KEYWORDVAR =="AAA39 PASSWORD RESET TS3",123,	
	ifelse (ETC$KEYWORDVAR =="AHN99 MESSAGE",124,	
	ifelse (ETC$KEYWORDVAR =="AAA13 PASSWORD RESET WINDOWS",125,	
	ifelse (ETC$KEYWORDVAR =="AAA16 PASSWORD PHONE",126,	
	ifelse (ETC$KEYWORDVAR =="AAA48 PASSWORD RESET AUTO",127,	
	ifelse (ETC$KEYWORDVAR =="AAF03 MAIL DOWN NOTWORK",128,	
	ifelse (ETC$KEYWORDVAR =="AAK08 INSTALL VISIO",129,	
	ifelse (ETC$KEYWORDVAR =="ACH01 DELL ACESS CONNECT",130,	
	ifelse (ETC$KEYWORDVAR =="AHI99 GEAR",131,	
	ifelse (ETC$KEYWORDVAR =="AHV99 RESET",132,	
	ifelse (ETC$KEYWORDVAR =="AAA99 PASSWORD",133,	
	ifelse (ETC$KEYWORDVAR =="AAA15 PASSWORD RESET PHONE",134,	
	ifelse (ETC$KEYWORDVAR =="AAH23 LOGIN NOT WORK VPN",135,	
	ifelse (ETC$KEYWORDVAR =="AAK03 INSTALL FIS",136,	
	ifelse (ETC$KEYWORDVAR =="ABM02 BW DOWN NOTWORK",137,	
	ifelse (ETC$KEYWORDVAR =="ACO99 WEBSITE",138,	
	ifelse (ETC$KEYWORDVAR =="AAC04 WEBEX LOGIN",139,	
	ifelse (ETC$KEYWORDVAR =="AAC17 WEBEX CONNECT",140,	
	ifelse (ETC$KEYWORDVAR =="AAB03 SAP LOGIN",141,	
	ifelse (ETC$KEYWORDVAR =="AEI99 MOUSE",142,	
	ifelse (ETC$KEYWORDVAR =="ADX01 WIRELESS ACESS CONNECT",143,	
	ifelse (ETC$KEYWORDVAR =="AAA19 PASSWORD RESET ACCOUNT",144,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAF05 MAIL INSTALL",145,	
	ifelse (ETC$KEYWORDVAR =="AAK09 INSTALL GOOGLE",146,	
	ifelse (ETC$KEYWORDVAR =="AAK29 INSTALL EXPENS",147,	
	ifelse (ETC$KEYWORDVAR =="AAL20 SETUP WIRELESS",148,	
	ifelse (ETC$KEYWORDVAR =="AAP02 SERVER DOWN NOTWORK",149,	
	ifelse (ETC$KEYWORDVAR =="AAQ10 NETWORK COMPUTER",150,	
	ifelse (ETC$KEYWORDVAR =="AAW99 VM",151,	
	ifelse (ETC$KEYWORDVAR =="ABV99 GOOGLE",152,	
	ifelse (ETC$KEYWORDVAR =="AFP99 FTP",153,	
	ifelse (ETC$KEYWORDVAR =="AGG01 PC ACESS CONNECT",154,	
	ifelse (ETC$KEYWORDVAR =="AAC99 WEBEX",155,	
	ifelse (ETC$KEYWORDVAR =="ADQ99 REPORT",156,	
	ifelse (ETC$KEYWORDVAR =="AAE05 EMAIL INSTALL",157,	
	ifelse (ETC$KEYWORDVAR =="AAK02 INSTALL EPDM",158,	
	ifelse (ETC$KEYWORDVAR =="AAK31 INSTALL OS",159,	
	ifelse (ETC$KEYWORDVAR =="ACB02 OFFICE DOWN NOTWORK",160,	
	ifelse (ETC$KEYWORDVAR =="AGF99 DESKTOP",161,	
	ifelse (ETC$KEYWORDVAR =="AAC05 WEBEX INSTALL",162,	
	ifelse (ETC$KEYWORDVAR =="ADN99 CPU",163,	
	ifelse (ETC$KEYWORDVAR =="AAD99 VOICEMAIL",164,	
	ifelse (ETC$KEYWORDVAR =="AAA01 PASSWORD RESET EMAIL",165,	
	ifelse (ETC$KEYWORDVAR =="AAA23 PASSWORD RESET USER",166,	
	ifelse (ETC$KEYWORDVAR =="AAK10 INSTALL OUTLOOK",167,	
	ifelse (ETC$KEYWORDVAR =="AAO01 VPN ACESS CONNECT",168,	
	ifelse (ETC$KEYWORDVAR =="ABE01 TOOL DOWN NOTWORK",169,	
	ifelse (ETC$KEYWORDVAR =="ABO99 FUSIONOPS",170,	
	ifelse (ETC$KEYWORDVAR =="ACI01 CISCO ACESS CONNECT",171,	
	ifelse (ETC$KEYWORDVAR =="ACO01 WEBSITE ACESS CONNECT",172,	
	ifelse (ETC$KEYWORDVAR =="ADB11 PHONE DIRECTORY",173,	
	ifelse (ETC$KEYWORDVAR =="AEH99 MONITOR",174,	
	ifelse (ETC$KEYWORDVAR =="AET99 EXPENSE",175,	
	ifelse (ETC$KEYWORDVAR =="AAH30 LOGIN ISSUE",176,	
	ifelse (ETC$KEYWORDVAR =="AAE25 EMAIL ISSUE",177,	
	ifelse (ETC$KEYWORDVAR =="AAG12 PRINT CHANGE",178,	
	ifelse (ETC$KEYWORDVAR =="AAB32 SAP PORTAL",179,	
	ifelse (ETC$KEYWORDVAR =="AAE03 EMAIL DOWN NOTWORK",180,	
	ifelse (ETC$KEYWORDVAR =="AAH13 LOGIN NOT WORK LAPTOP",181,	
	ifelse (ETC$KEYWORDVAR =="ABY99 VISIO",182,	
	ifelse (ETC$KEYWORDVAR =="AAH99 LOGIN",183,	
	ifelse (ETC$KEYWORDVAR =="AAJ99 LOCK",184,	
	ifelse (ETC$KEYWORDVAR =="AAK30 INSTALL IE",185,	
	ifelse (ETC$KEYWORDVAR =="ACD01 OUTLOOK ACESS CONNECT",186,	
	ifelse (ETC$KEYWORDVAR =="ACX03 SERVICE DOWN NOTWORK",187,	
	ifelse (ETC$KEYWORDVAR =="ADX02 WIRELESS DOWN NOTWORK",188,	
	ifelse (ETC$KEYWORDVAR =="AFS99 INK",189,	
	ifelse (ETC$KEYWORDVAR =="AAN01 INTERNET ACESS CONNECT",190,	
	ifelse (ETC$KEYWORDVAR =="AFG99 DELIVER",191,	
	ifelse (ETC$KEYWORDVAR =="AAG05 PRINT INSTALL",192,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ADW99 DOMAIN",193,	
	ifelse (ETC$KEYWORDVAR =="AAK04 INSTALL VPN",194,	
	ifelse (ETC$KEYWORDVAR =="ABJ01 TS3 ACESS CONNECT",195,	
	ifelse (ETC$KEYWORDVAR =="AEM99 MEMORY",196,	
	ifelse (ETC$KEYWORDVAR =="ABD01 PTOOL DOWN NOTWORK",197,	
	ifelse (ETC$KEYWORDVAR =="AAP12 SERVER ISSUE",198,	
	ifelse (ETC$KEYWORDVAR =="ABI99 PLM",199,	
	ifelse (ETC$KEYWORDVAR =="AAO99 VPN",200,	
	ifelse (ETC$KEYWORDVAR =="ACD03 OUTLOOK ISSUE",201,	
	ifelse (ETC$KEYWORDVAR =="AAL31 SETUP COMPUTER",202,	
	ifelse (ETC$KEYWORDVAR =="AAA28 PASSWORD VPN",203,	
	ifelse (ETC$KEYWORDVAR =="AAA36 PASSWORD PCARD",204,	
	ifelse (ETC$KEYWORDVAR =="AAB10 SAP INSTALL",205,	
	ifelse (ETC$KEYWORDVAR =="AAC08 WEBEX CHANGE",206,	
	ifelse (ETC$KEYWORDVAR =="AAC14 WEBEX TOOL",207,	
	ifelse (ETC$KEYWORDVAR =="AAF02 MAIL ACCESS CONNECT",208,	
	ifelse (ETC$KEYWORDVAR =="AAH31 LOGIN ISSUE BW",209,	
	ifelse (ETC$KEYWORDVAR =="AAK19 INSTALL ALTIUM",210,	
	ifelse (ETC$KEYWORDVAR =="AAK24 INSTALL ENGINEER",211,	
	ifelse (ETC$KEYWORDVAR =="AAK26 INSTALL PDF",212,	
	ifelse (ETC$KEYWORDVAR =="AAK33 INSTALL SOFTWARE",213,	
	ifelse (ETC$KEYWORDVAR =="AAL15 SETUP DELL",214,	
	ifelse (ETC$KEYWORDVAR =="AAM01 CONFIG OUTLOOK",215,	
	ifelse (ETC$KEYWORDVAR =="AAU02 LINK DOWN NOTWORK",216,	
	ifelse (ETC$KEYWORDVAR =="ABE02 TOOL ISSUE",217,	
	ifelse (ETC$KEYWORDVAR =="ACC01 EXCEL ACESS CONNECT",218,	
	ifelse (ETC$KEYWORDVAR =="ACC02 EXCEL DOWN NOTWORK",219,	
	ifelse (ETC$KEYWORDVAR =="ACP01 SITE ACESS CONNECT",220,	
	ifelse (ETC$KEYWORDVAR =="ACZ12 NEW HIRE ACCOUNT",221,	
	ifelse (ETC$KEYWORDVAR =="ADB02 PHONE ACESS CONNECT",222,	
	ifelse (ETC$KEYWORDVAR =="ADO99 VOIP",223,	
	ifelse (ETC$KEYWORDVAR =="ADX03 WIRELESS ISSUE",224,	
	ifelse (ETC$KEYWORDVAR =="AEG99 KEYBOARD",225,	
	ifelse (ETC$KEYWORDVAR =="AGL99 MALWARE",226,	
	ifelse (ETC$KEYWORDVAR =="AHD99 ALERT",227,	
	ifelse (ETC$KEYWORDVAR =="AHG99 FIRM",228,	
	ifelse (ETC$KEYWORDVAR =="AIJ99 PAY",229,	
	ifelse (ETC$KEYWORDVAR =="ACD99 OUTLOOK",230,	
	ifelse (ETC$KEYWORDVAR =="ACH99 DELL",231,	
	ifelse (ETC$KEYWORDVAR =="AHQ99 PROCESS",232,	
	ifelse (ETC$KEYWORDVAR =="AAH24 LOGIN NOT WORK",233,	
	ifelse (ETC$KEYWORDVAR =="AAK15 INSTALL ADMIN",234,	
	ifelse (ETC$KEYWORDVAR =="AAE12 EMAIL CHANGE",235,	
	ifelse (ETC$KEYWORDVAR =="AAJ04 LOCK COMPUTER",236,	
	ifelse (ETC$KEYWORDVAR =="AAC03 WEBEX DOWN NOTWORK",237,	
	ifelse (ETC$KEYWORDVAR =="AAK13 INSTALL SCAN",238,	
	ifelse (ETC$KEYWORDVAR =="ACX10 SERVICE ACCOUNT",239,	
	ifelse (ETC$KEYWORDVAR =="AAV06 STATION ISSUE",240,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ACW13 SYSTEM ISSUE",241,	
	ifelse (ETC$KEYWORDVAR =="AII99 REFU",242,	
	ifelse (ETC$KEYWORDVAR =="AAG02 PRINT DOWN NOTWORK PROD",243,	
	ifelse (ETC$KEYWORDVAR =="AAH03 LOGIN NOT WORK COMPUTER",244,	
	ifelse (ETC$KEYWORDVAR =="AFV99 MEET",245,	
	ifelse (ETC$KEYWORDVAR =="AAZ01 JOB STEP1",246,	
	ifelse (ETC$KEYWORDVAR =="AAB35 SAP TS3",247,	
	ifelse (ETC$KEYWORDVAR =="AAB37 SAP BW",248,	
	ifelse (ETC$KEYWORDVAR =="AAF12 MAIL CHANGE",249,	
	ifelse (ETC$KEYWORDVAR =="AAF25 MAIL FULL",250,	
	ifelse (ETC$KEYWORDVAR =="AAK12 INSTALL PROJECT",251,	
	ifelse (ETC$KEYWORDVAR =="AAK23 INSTALL DRIVE",252,	
	ifelse (ETC$KEYWORDVAR =="ABJ03 TS3 ISSUE",253,	
	ifelse (ETC$KEYWORDVAR =="ABU99 ADOBE",254,	
	ifelse (ETC$KEYWORDVAR =="ACF99 INTRANET",255,	
	ifelse (ETC$KEYWORDVAR =="ACP02 SITE DOWN NOTWORK",256,	
	ifelse (ETC$KEYWORDVAR =="ADP99 USB",257,	
	ifelse (ETC$KEYWORDVAR =="ADY01 WIFI ACESS CONNECT",258,	
	ifelse (ETC$KEYWORDVAR =="AEA01 LAN ACESS CONNECT",259,	
	ifelse (ETC$KEYWORDVAR =="ACU01 DRIVE ACESS CONNECT",260,	
	ifelse (ETC$KEYWORDVAR =="ADR99 SOFTWARE",261,	
	ifelse (ETC$KEYWORDVAR =="AAC12 WEBEX MEETING",262,	
	ifelse (ETC$KEYWORDVAR =="AAG01 PRINT ACCESS CONNECT",263,	
	ifelse (ETC$KEYWORDVAR =="AAP01 SERVER ACESS CONNECT",264,	
	ifelse (ETC$KEYWORDVAR =="AAE10 EMAIL PHONE",265,	
	ifelse (ETC$KEYWORDVAR =="AAP07 SERVER FILE",266,	
	ifelse (ETC$KEYWORDVAR =="ABG01 FIS ACESS CONNECT",267,	
	ifelse (ETC$KEYWORDVAR =="AFR99 TONER",268,	
	ifelse (ETC$KEYWORDVAR =="AHM99 LICENSE",269,	
	ifelse (ETC$KEYWORDVAR =="ABT99 PDF",270,	
	ifelse (ETC$KEYWORDVAR =="ACI99 CISCO",271,	
	ifelse (ETC$KEYWORDVAR =="AFK99 GROUP",272,	
	ifelse (ETC$KEYWORDVAR =="AIR99 TEST",273,	
	ifelse (ETC$KEYWORDVAR =="AAE07 EMAIL ACCOUNT",274,	
	ifelse (ETC$KEYWORDVAR =="AAF99 MAIL",275,	
	ifelse (ETC$KEYWORDVAR =="AAK07 INSTALL WINDOWS",276,	
	ifelse (ETC$KEYWORDVAR =="AAO02 VPN DOWN NOTWORK",277,	
	ifelse (ETC$KEYWORDVAR =="ABG99 FIS",278,	
	ifelse (ETC$KEYWORDVAR =="ACU02 DRIVE DOWN NOTWORK",279,	
	ifelse (ETC$KEYWORDVAR =="ACZ19 USER ACESS CONNECT",280,	
	ifelse (ETC$KEYWORDVAR =="ADB08 PHONE CHANGE",281,	
	ifelse (ETC$KEYWORDVAR =="ADV02 PORTAL DOWN NOTWORK",282,	
	ifelse (ETC$KEYWORDVAR =="ADZ01 REMOTE ACESS CONNECT",283,	
	ifelse (ETC$KEYWORDVAR =="AGU99 HELP DESK",284,	
	ifelse (ETC$KEYWORDVAR =="ACZ14 NEW HIRE",285,	
	ifelse (ETC$KEYWORDVAR =="AGA99 BATTERY",286,	
	ifelse (ETC$KEYWORDVAR =="AAE01 EMAIL DELIVERY",287,	
	ifelse (ETC$KEYWORDVAR =="AIO99 OS",288,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAN02 INTERNET DOWN NOTWORK",289,	
	ifelse (ETC$KEYWORDVAR =="ABQ01 ENGINEER ACESS CONNECT",290,	
	ifelse (ETC$KEYWORDVAR =="ACM99 SQL",291,	
	ifelse (ETC$KEYWORDVAR =="AEB99 BOX",292,	
	ifelse (ETC$KEYWORDVAR =="AAN99 INTERNET",293,	
	ifelse (ETC$KEYWORDVAR =="ABS99 PROJECT",294,	
	ifelse (ETC$KEYWORDVAR =="ACC99 EXCEL",295,	
	ifelse (ETC$KEYWORDVAR =="ADA07 NEW COMPUTER",296,	
	ifelse (ETC$KEYWORDVAR =="ADX99 WIRELESS",297,	
	ifelse (ETC$KEYWORDVAR =="AGP99 ACCESS",298,	
	ifelse (ETC$KEYWORDVAR =="AAQ01 NETWORK WIRELESS",299,	
	ifelse (ETC$KEYWORDVAR =="AAQ03 NETWORK DOWN NOTWORK",300,	
	ifelse (ETC$KEYWORDVAR =="ABJ99 TS3",301,	
	ifelse (ETC$KEYWORDVAR =="ABM01 BW ACESS CONNECT",302,	
	ifelse (ETC$KEYWORDVAR =="ADB03 PHONE DOWN NOTWORK",303,	
	ifelse (ETC$KEYWORDVAR =="ACD02 OUTLOOK DOWN NOTWORK",304,	
	ifelse (ETC$KEYWORDVAR =="AAC15 WEBEX ISSUE",305,	
	ifelse (ETC$KEYWORDVAR =="AHT99 RECORD",306,	
	ifelse (ETC$KEYWORDVAR =="ADB99 PHONE",307,	
	ifelse (ETC$KEYWORDVAR =="AAE99 EMAIL",308,	
	ifelse (ETC$KEYWORDVAR =="AEU99 PORT",309,	
	ifelse (ETC$KEYWORDVAR =="AIE99 UPLOAD",310,	
	ifelse (ETC$KEYWORDVAR =="AAK99 INSTALL",311,	
	ifelse (ETC$KEYWORDVAR =="AAQ02 NETWORK ACESS CONNECT",312,	
	ifelse (ETC$KEYWORDVAR =="AAG20 PRINT ISSUE",313,	
	ifelse (ETC$KEYWORDVAR =="AAV99 STATION",314,	
	ifelse (ETC$KEYWORDVAR =="ACV99 FILE",315,	
	ifelse (ETC$KEYWORDVAR =="AAB23 SAP EXCEL",316,	
	ifelse (ETC$KEYWORDVAR =="AAC10 WEBEX SETTING",317,	
	ifelse (ETC$KEYWORDVAR =="AAE04 EMAIL LOGIN",318,	
	ifelse (ETC$KEYWORDVAR =="AAE09 EMAIL SCAN",319,	
	ifelse (ETC$KEYWORDVAR =="AAE19 EMAIL JUNK",320,	
	ifelse (ETC$KEYWORDVAR =="AAE20 EMAIL OUTLOOK",321,	
	ifelse (ETC$KEYWORDVAR =="AAF20 MAIL OUTLOOK",322,	
	ifelse (ETC$KEYWORDVAR =="AAF26 MAIL ISSUE",323,	
	ifelse (ETC$KEYWORDVAR =="AAH04 LOGIN NOT WORK DELL",324,	
	ifelse (ETC$KEYWORDVAR =="AAH40 LOGIN ISSUE FIS",325,	
	ifelse (ETC$KEYWORDVAR =="AAH51 LOGIN ISSUE TOMCAT",326,	
	ifelse (ETC$KEYWORDVAR =="AAK01 INSTALL PLM",327,	
	ifelse (ETC$KEYWORDVAR =="AAK22 INSTALL DELL",328,	
	ifelse (ETC$KEYWORDVAR =="AAL05 SETUP OUTLOOK",329,	
	ifelse (ETC$KEYWORDVAR =="AAL09 SETUP ADMIN",330,	
	ifelse (ETC$KEYWORDVAR =="AAL22 SETUP STATION",331,	
	ifelse (ETC$KEYWORDVAR =="AAL27 SETUP TS3",332,	
	ifelse (ETC$KEYWORDVAR =="AAN03 INTERNET ISSUE",333,	
	ifelse (ETC$KEYWORDVAR =="AAP04 SERVER BACKUP",334,	
	ifelse (ETC$KEYWORDVAR =="AAS01 SWITCH ACESS CONNECT",335,	
	ifelse (ETC$KEYWORDVAR =="AAV05 STATION COMPUTER",336,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAW01 VM ACESS CONNECT",337,	
	ifelse (ETC$KEYWORDVAR =="ABI01 PLM ACESS CONNECT",338,	
	ifelse (ETC$KEYWORDVAR =="ABI03 PLM ISSUE",339,	
	ifelse (ETC$KEYWORDVAR =="ABN03 BOM ISSUE",340,	
	ifelse (ETC$KEYWORDVAR =="ABP99 ALTIUM",341,	
	ifelse (ETC$KEYWORDVAR =="ABR02 SHAREPOINT DOWN NOTWORK",342,	
	ifelse (ETC$KEYWORDVAR =="ABZ02 SALESFORCE DOWN NOTWORK",343,	
	ifelse (ETC$KEYWORDVAR =="ACF02 INTRANET DOWN NOTWORK",344,	
	ifelse (ETC$KEYWORDVAR =="ACO02 WEBSITE DOWN NOTWORK",345,	
	ifelse (ETC$KEYWORDVAR =="ACP03 SITE ISSUE",346,	
	ifelse (ETC$KEYWORDVAR =="ACQ99 MCAFEE",347,	
	ifelse (ETC$KEYWORDVAR =="ACV01 FILE ACESS CONNECT",348,	
	ifelse (ETC$KEYWORDVAR =="ACX13 SERVICE ISSUE",349,	
	ifelse (ETC$KEYWORDVAR =="ACZ15 USERS ACESS CONNECT",350,	
	ifelse (ETC$KEYWORDVAR =="ACZ17 USERS ISSUE",351,	
	ifelse (ETC$KEYWORDVAR =="ADA08 NEW ISSUE",352,	
	ifelse (ETC$KEYWORDVAR =="ADD02 FAX DOWN NOTWORK",353,	
	ifelse (ETC$KEYWORDVAR =="ADD09 FAX ISSUE",354,	
	ifelse (ETC$KEYWORDVAR =="ADE04 SCAN COMPUTER",355,	
	ifelse (ETC$KEYWORDVAR =="ADG01 CRITICAL ALERT",356,	
	ifelse (ETC$KEYWORDVAR =="ADJ99 SQ",357,	
	ifelse (ETC$KEYWORDVAR =="ADT99 INTERFACE",358,	
	ifelse (ETC$KEYWORDVAR =="ADW01 DOMAIN ACESS CONNECT",359,	
	ifelse (ETC$KEYWORDVAR =="ADZ99 REMOTE",360,	
	ifelse (ETC$KEYWORDVAR =="AEJ99 HEADSET",361,	
	ifelse (ETC$KEYWORDVAR =="AEN99 SPACE",362,	
	ifelse (ETC$KEYWORDVAR =="AFH99 FIREWALL",363,	
	ifelse (ETC$KEYWORDVAR =="AFI99 BARCODE",364,	
	ifelse (ETC$KEYWORDVAR =="AGG02 PC DOWN NOTWORK",365,	
	ifelse (ETC$KEYWORDVAR =="AGG03 PC ISSUE",366,	
	ifelse (ETC$KEYWORDVAR =="AGW99 QUICKEN",367,	
	ifelse (ETC$KEYWORDVAR =="AGY99 DONGLE",368,	
	ifelse (ETC$KEYWORDVAR =="AHS99 PRODUCT",369,	
	ifelse (ETC$KEYWORDVAR =="AIG99 WEB",370,	
	ifelse (ETC$KEYWORDVAR =="AIN99 CAR",371,	
	ifelse (ETC$KEYWORDVAR =="AAB08 SAP ACCOUNT",372,	
	ifelse (ETC$KEYWORDVAR =="AAG03 PRINT DOWN NOTWORK",373,	
	ifelse (ETC$KEYWORDVAR =="ACT01 FOLDER ACESS CONNECT",374,	
	ifelse (ETC$KEYWORDVAR =="ABD02 PTOOL ISSUE",375,	
	ifelse (ETC$KEYWORDVAR =="ACZ99 USER",376,	
	ifelse (ETC$KEYWORDVAR =="ADA04 NEW SOFTWARE",377,	
	ifelse (ETC$KEYWORDVAR =="ABD99 PTOOL",378,	
	ifelse (ETC$KEYWORDVAR =="ABK99 REFUSOL",379,	
	ifelse (ETC$KEYWORDVAR =="ACW03 SYSTEM DOWN NOTWORK",380,	
	ifelse (ETC$KEYWORDVAR =="ACY10 APP ACCOUNT",381,	
	ifelse (ETC$KEYWORDVAR =="AGQ99 ADMIN",382,	
	ifelse (ETC$KEYWORDVAR =="AAB02 SAP DOWN NOTWORK",383,	
	ifelse (ETC$KEYWORDVAR =="AAO03 VPN ISSUE",384,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AIP99 IE",385,	
	ifelse (ETC$KEYWORDVAR =="AAB01 SAP ACESS CONNECT",386,	
	ifelse (ETC$KEYWORDVAR =="AAL12 SETUP PHONE",387,	
	ifelse (ETC$KEYWORDVAR =="ADA06 NEW PHONE",388,	
	ifelse (ETC$KEYWORDVAR =="ADC99 COPIER",389,	
	ifelse (ETC$KEYWORDVAR =="ACU99 DRIVE",390,	
	ifelse (ETC$KEYWORDVAR =="ADB12 PHONE ISSUE",391,	
	ifelse (ETC$KEYWORDVAR =="AEQ99 PTO",392,	
	ifelse (ETC$KEYWORDVAR =="AAG15 PRINT LABEL",393,	
	ifelse (ETC$KEYWORDVAR =="ABG02 FIS DOWN NOTWORK",394,	
	ifelse (ETC$KEYWORDVAR =="ABH01 EPDM ACESS CONNECT",395,	
	ifelse (ETC$KEYWORDVAR =="ACY11 APP ADMIN",396,	
	ifelse (ETC$KEYWORDVAR =="ADD99 FAX",397,	
	ifelse (ETC$KEYWORDVAR =="AGG99 PC",398,	
	ifelse (ETC$KEYWORDVAR =="AGO99 DOWN",399,	
	ifelse (ETC$KEYWORDVAR =="ADF01 INCIDENT ATTACK",400,	
	ifelse (ETC$KEYWORDVAR =="AGD99 COMPUTER",401,	
	ifelse (ETC$KEYWORDVAR =="AGD02 COMPUTER DOWN NOTWORK",402,	
	ifelse (ETC$KEYWORDVAR =="ACB99 OFFICE",403,	
	ifelse (ETC$KEYWORDVAR =="ADF99 INCIDENT",404,	
	ifelse (ETC$KEYWORDVAR =="AAB06 SAP NEW REQUEST",405,	
	ifelse (ETC$KEYWORDVAR =="AAE02 EMAIL ACCESS CONNECT",406,	
	ifelse (ETC$KEYWORDVAR =="AAQ99 NETWORK",407,	
	ifelse (ETC$KEYWORDVAR =="ACK02 CAD DOWN NOTWORK",408,	
	ifelse (ETC$KEYWORDVAR =="ACU03 DRIVE ISSUE",409,	
	ifelse (ETC$KEYWORDVAR =="AGK99 VIRUS",410,	
	ifelse (ETC$KEYWORDVAR =="AHO99 OUTPUT",411,	
	ifelse (ETC$KEYWORDVAR =="AIH99 TICKET",412,	
	ifelse (ETC$KEYWORDVAR =="AIQ99 TRAINING",413,	
	ifelse (ETC$KEYWORDVAR =="ABE99 TOOL",414,	
	ifelse (ETC$KEYWORDVAR =="OTHER",415,	
	ifelse (ETC$KEYWORDVAR =="AAI99 BLOCK",416,	
	ifelse (ETC$KEYWORDVAR =="AAU99 LINK",417,	
	ifelse (ETC$KEYWORDVAR =="AGH03 TIME ISSUE",418,	
	ifelse (ETC$KEYWORDVAR =="AGT99 ISSUE",419,	
	ifelse (ETC$KEYWORDVAR =="AAB40 SAP ISSUE",420,	
	ifelse (ETC$KEYWORDVAR =="AEL99 RAM",421,	
	ifelse (ETC$KEYWORDVAR =="ACT99 FOLDER",422,	
	ifelse (ETC$KEYWORDVAR =="ADH99 REQ",423,	
	ifelse (ETC$KEYWORDVAR =="AAB99 SAP",424,	
	ifelse (ETC$KEYWORDVAR =="AAE26 EMAIL REQUEST",425,	
	ifelse (ETC$KEYWORDVAR =="AAF10 MAIL PHONE",426,	
	ifelse (ETC$KEYWORDVAR =="AAL02 SETUP VPN",427,	
	ifelse (ETC$KEYWORDVAR =="AAL99 SETUP",428,	
	ifelse (ETC$KEYWORDVAR =="AAP09 SERVER ACCOUNT",429,	
	ifelse (ETC$KEYWORDVAR =="ACA99 MS OFFICE",430,	
	ifelse (ETC$KEYWORDVAR =="ACF01 INTRANET ACESS CONNECT",431,	
	ifelse (ETC$KEYWORDVAR =="ACR99 SOLIDWORKS",432,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ADL99 ITCHECK",433,	
	ifelse (ETC$KEYWORDVAR =="ADV03 PORTAL ISSUE",434,	
	ifelse (ETC$KEYWORDVAR =="AFN99 ROLE",435,	
	ifelse (ETC$KEYWORDVAR =="AFY99 CALENDAR",436,	
	ifelse (ETC$KEYWORDVAR =="AAP99 SERVER",437,	
	ifelse (ETC$KEYWORDVAR =="AFX99 CALL",438,	
	ifelse (ETC$KEYWORDVAR =="ABG03 FIS ISSUE",439,	
	ifelse (ETC$KEYWORDVAR =="ABH03 EPDM ISSUE",440,	
	ifelse (ETC$KEYWORDVAR =="ABK01 REFUSOL ACESS CONNECT",441,	
	ifelse (ETC$KEYWORDVAR =="ACX02 SERVICE ACESS CONNECT",442,	
	ifelse (ETC$KEYWORDVAR =="AGS99 CHANGE",443,	
	ifelse (ETC$KEYWORDVAR =="AAG07 PRINT INK",444,	
	ifelse (ETC$KEYWORDVAR =="AAG09 FIX PRINTER",445,	
	ifelse (ETC$KEYWORDVAR =="AAT02 NODE DOWN NOTWORK",446,	
	ifelse (ETC$KEYWORDVAR =="ABL01 ASTEA ACESS CONNECT",447,	
	ifelse (ETC$KEYWORDVAR =="ACZ20 USER DOWN NOTWORK",448,	
	ifelse (ETC$KEYWORDVAR =="AHF99 DIRECTORY",449,	
	ifelse (ETC$KEYWORDVAR =="ACY02 APP ACESS CONNECT",450,	
	ifelse (ETC$KEYWORDVAR =="AEO99 VOLUME",451,	
	ifelse (ETC$KEYWORDVAR =="AAT03 NODE REBOOT",452,	
	ifelse (ETC$KEYWORDVAR =="ACP99 SITE",453,	
	ifelse (ETC$KEYWORDVAR =="AAB41 SAP REQUEST",454,	
	ifelse (ETC$KEYWORDVAR =="AAG99 PRINT",455,	
	ifelse (ETC$KEYWORDVAR =="ACV02 FILE DOWN NOTWORK",456,	
	ifelse (ETC$KEYWORDVAR =="AFU99 INFO",457,	
	ifelse (ETC$KEYWORDVAR =="ACE99 EXCHANGE",458,	
	ifelse (ETC$KEYWORDVAR =="ACX99 SERVICE",459,	
	ifelse (ETC$KEYWORDVAR =="AES99 WG",460,	
	ifelse (ETC$KEYWORDVAR =="AGB99 POWER",461,	
	ifelse (ETC$KEYWORDVAR =="AAX01 EQUIPMENT FOLLOWUP",462,	
	ifelse (ETC$KEYWORDVAR =="ACY03 APP DOWN NOTWORK",463,	
	ifelse (ETC$KEYWORDVAR =="ABR99 SHAREPOINT",464,	
	ifelse (ETC$KEYWORDVAR =="ADV99 PORTAL",465,	
	ifelse (ETC$KEYWORDVAR =="ACW99 SYSTEM",466,	
	ifelse (ETC$KEYWORDVAR =="AFD99 SALE",467,	
	ifelse (ETC$KEYWORDVAR =="AGH99 TIME",468,	
	ifelse (ETC$KEYWORDVAR =="ABH99 EPDM",469,	
	ifelse (ETC$KEYWORDVAR =="AAE08 EMAIL PRINT",470,	
	ifelse (ETC$KEYWORDVAR =="AAE14 EMAIL NOTIFICATION",471,	
	ifelse (ETC$KEYWORDVAR =="AAF04 MAIL LOGIN",472,	
	ifelse (ETC$KEYWORDVAR =="AAH26 LOGIN LONG",473,	
	ifelse (ETC$KEYWORDVAR =="AAH28 LOGIN CHANGE",474,	
	ifelse (ETC$KEYWORDVAR =="AAH33 LOGIN ISSUE COMPUTER",475,	
	ifelse (ETC$KEYWORDVAR =="AAL26 SETUP TECHNICIAN",476,	
	ifelse (ETC$KEYWORDVAR =="AAM04 CONFIG PHONE",477,	
	ifelse (ETC$KEYWORDVAR =="AAP05 SERVER RESTORE",478,	
	ifelse (ETC$KEYWORDVAR =="AAQ14 NETWORK ISSUE",479,	
	ifelse (ETC$KEYWORDVAR =="AAS05 SWITCH ISSUE",480,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAT99 NODE",481,	
	ifelse (ETC$KEYWORDVAR =="AAV02 STATION DOWN NOTWORK",482,	
	ifelse (ETC$KEYWORDVAR =="AAX03 EQUIPMENT ISSUE",483,	
	ifelse (ETC$KEYWORDVAR =="ABF01 SFDC DOWN NOTWORK",484,	
	ifelse (ETC$KEYWORDVAR =="ABX99 ADP",485,	
	ifelse (ETC$KEYWORDVAR =="ACB01 OFFICE ACESS CONNECT",486,	
	ifelse (ETC$KEYWORDVAR =="ACC03 EXCEL ISSUE",487,	
	ifelse (ETC$KEYWORDVAR =="ACG99 EXTRANET",488,	
	ifelse (ETC$KEYWORDVAR =="ACM01 SQL ACESS CONNECT",489,	
	ifelse (ETC$KEYWORDVAR =="ACZ21 USER ISSUE",490,	
	ifelse (ETC$KEYWORDVAR =="ADG99 CRITICAL",491,	
	ifelse (ETC$KEYWORDVAR =="ADM99 FLASH",492,	
	ifelse (ETC$KEYWORDVAR =="AFM99 LIST",493,	
	ifelse (ETC$KEYWORDVAR =="ABM99 BW",494,	
	ifelse (ETC$KEYWORDVAR =="ADA02 NEW GENERAL",495,	
	ifelse (ETC$KEYWORDVAR =="AAZ99 JOB",496,	
	ifelse (ETC$KEYWORDVAR =="ACY99 APP",497,	
	ifelse (ETC$KEYWORDVAR =="AAB16 SAP CHANGE",498,	
	ifelse (ETC$KEYWORDVAR =="ABZ99 SALESFORCE",499,	
	ifelse (ETC$KEYWORDVAR =="AEY99 ORDER",500,	
	ifelse (ETC$KEYWORDVAR =="AFT99 DOC",501,	
	ifelse (ETC$KEYWORDVAR =="AAF07 MAIL ACCOUNT",502,	
	ifelse (ETC$KEYWORDVAR =="ABL03 ASTEA ISSUE",503,	
	ifelse (ETC$KEYWORDVAR =="ACW02 SYSTEM ACESS CONNECT",504,	
	ifelse (ETC$KEYWORDVAR =="AER99 DASHBOARD",505,	
	ifelse (ETC$KEYWORDVAR =="AFL99 CASE",506,	
	ifelse (ETC$KEYWORDVAR =="AEX99 SHIP",507,	
	ifelse (ETC$KEYWORDVAR =="ACY13 APP ISSUE",508,	
	ifelse (ETC$KEYWORDVAR =="AEF99 MOVE",509,	
	ifelse (ETC$KEYWORDVAR =="AGD01 COMPUTER ACESS CONNECT",510,	
	ifelse (ETC$KEYWORDVAR =="AAB09 SAP PRINT",511,	
	ifelse (ETC$KEYWORDVAR =="ABQ99 ENGINEER",512,	
	ifelse (ETC$KEYWORDVAR =="AFC99 QUOTE",513,	
	ifelse (ETC$KEYWORDVAR =="AFJ99 CODE",514,	
	ifelse (ETC$KEYWORDVAR =="AID99 TRACK",515,	
	ifelse (ETC$KEYWORDVAR =="AAB10 SAP CODE",516,	
	ifelse (ETC$KEYWORDVAR =="AAB17 SAP AUTH",517,	
	ifelse (ETC$KEYWORDVAR =="ABB99 WINDOWS",518,	
	ifelse (ETC$KEYWORDVAR =="AAG16 PRINT INVOICE",519,	
	ifelse (ETC$KEYWORDVAR =="AAK11 INSTALL OFFICE",520,	
	ifelse (ETC$KEYWORDVAR =="AAP10 SERVER ADMIN",521,	
	ifelse (ETC$KEYWORDVAR =="AAU05 LINK ISSUE",522,	
	ifelse (ETC$KEYWORDVAR =="ABL02 ASTEA DOWN NOTWORK",523,	
	ifelse (ETC$KEYWORDVAR =="ABL99 ASTEA",524,	
	ifelse (ETC$KEYWORDVAR =="ABS02 PROJECT DOWN NOTWORK",525,	
	ifelse (ETC$KEYWORDVAR =="ACB03 OFFICE ISSUE",526,	
	ifelse (ETC$KEYWORDVAR =="ACH03 DELL ISSUE",527,	
	ifelse (ETC$KEYWORDVAR =="ACK99 CAD",528,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ADB09 PHONE DELIVERY",529,	
	ifelse (ETC$KEYWORDVAR =="ADE99 SCAN",530,	
	ifelse (ETC$KEYWORDVAR =="AEW99 PART",531,	
	ifelse (ETC$KEYWORDVAR =="AFO99 BACKUP",532,	
	ifelse (ETC$KEYWORDVAR =="AFQ99 EFT",533,	
	ifelse (ETC$KEYWORDVAR =="AGX99 ENERGY",534,	
	ifelse (ETC$KEYWORDVAR =="AHR99 PRODUCTION",535,	
	ifelse (ETC$KEYWORDVAR =="AHU99 RECRUIT",536,	
	ifelse (ETC$KEYWORDVAR =="ACZ18 USERS",537,	
	ifelse (ETC$KEYWORDVAR =="AAM07 CONFIG COMPUTER",538,	
	ifelse (ETC$KEYWORDVAR =="ABR01 SHAREPOINT ACESS CONNECT",539,	
	ifelse (ETC$KEYWORDVAR =="AAB07 SAP NEW ACCOUNT",540,	
	ifelse (ETC$KEYWORDVAR =="AAB15 SAP REPORT",541,	
	ifelse (ETC$KEYWORDVAR =="AAI02 BLOCK SITE",542,	
	ifelse (ETC$KEYWORDVAR =="AAU01 LINK ACESS CONNECT",543,	
	ifelse (ETC$KEYWORDVAR =="ABR03 SHAREPOINT ISSUE",544,	
	ifelse (ETC$KEYWORDVAR =="AFF99 BILL",545,	
	ifelse (ETC$KEYWORDVAR =="AIL99 MAT",546,	
	ifelse (ETC$KEYWORDVAR =="AAB13 SAP ITEM",547,	
	ifelse (ETC$KEYWORDVAR =="AAG08 NEW PRINTER",548,	
	ifelse (ETC$KEYWORDVAR =="AAK16 INSTALL CAD",549,	
	ifelse (ETC$KEYWORDVAR =="AAM99 CONFIG",550,	
	ifelse (ETC$KEYWORDVAR =="AAS99 SWITCH",551,	
	ifelse (ETC$KEYWORDVAR =="AAY99 INVENTORY",552,	
	ifelse (ETC$KEYWORDVAR =="ADB04 FIX PHONE",553,	
	ifelse (ETC$KEYWORDVAR =="ADE02 SCAN DOWN NOTWORK",554,	
	ifelse (ETC$KEYWORDVAR =="ADV01 PORTAL ACESS CONNECT",555,	
	ifelse (ETC$KEYWORDVAR =="AFW99 CONFERENCE",556,	
	ifelse (ETC$KEYWORDVAR =="AGZ99 DATABASE",557,	
	ifelse (ETC$KEYWORDVAR =="ABC99 WIN",558,	
	ifelse (ETC$KEYWORDVAR =="AEV99 MATERIAL",559,	
	ifelse (ETC$KEYWORDVAR =="AAI01 BLOCK VENDER",560,	
	ifelse (ETC$KEYWORDVAR =="ABS01 PROJECT ACESS CONNECT",561,	
	ifelse (ETC$KEYWORDVAR =="ADA99 NEW",562,	
	ifelse (ETC$KEYWORDVAR =="ADH01 DEBUG REQ",563,	
	ifelse (ETC$KEYWORDVAR =="AIK99 LOG",564,	
	ifelse (ETC$KEYWORDVAR =="AFA99 INVOICE",565,	
	ifelse (ETC$KEYWORDVAR =="AAA37 PASSWORD RESET PC",566,	
	ifelse (ETC$KEYWORDVAR =="AAB33 SAP REFUSOL",567,	
	ifelse (ETC$KEYWORDVAR =="AAP08 SERVER ROLLOUT",568,	
	ifelse (ETC$KEYWORDVAR =="AAV01 STATION ACESS CONNECT",569,	
	ifelse (ETC$KEYWORDVAR =="ABF02 SFDC ISSUE",570,	
	ifelse (ETC$KEYWORDVAR =="ABF99 SFDC",571,	
	ifelse (ETC$KEYWORDVAR =="ACM02 SQL DOWN NOTWORK",572,	
	ifelse (ETC$KEYWORDVAR =="AGF02 DESKTOP DOWN NOTWORK",573,	
	ifelse (ETC$KEYWORDVAR =="AAX99 EQUIPMENT",574,	
	ifelse (ETC$KEYWORDVAR =="AEA99 LAN",575,	
	ifelse (ETC$KEYWORDVAR =="AFZ99 ADDRESS",576,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ABN99 BOM",577,	
	ifelse (ETC$KEYWORDVAR =="ADE10 SCAN ISSUE",578,	
	ifelse (ETC$KEYWORDVAR =="AFB99 PURCHASE",579,	
	ifelse (ETC$KEYWORDVAR =="AHY99 SCREEN",580,	
	ifelse (ETC$KEYWORDVAR =="AAB11 SAP ORDER",581,	
	ifelse (ETC$KEYWORDVAR =="ADA01 NEW HARDWARE",582,	
	ifelse (ETC$KEYWORDVAR =="AHA99 DATA",583,	
	ifelse (ETC$KEYWORDVAR =="AGD03 COMPUTER ISSUE",584,	
	ifelse (ETC$KEYWORDVAR =="ADA06 NEW SNC",585,	
	ifelse (ETC$KEYWORDVAR =="ABM03 BW ISSUE",586,	
	ifelse (ETC$KEYWORDVAR =="AFE99 TAX",587,	
	ifelse (ETC$KEYWORDVAR =="ADI99 SNC",588,	
	ifelse (ETC$KEYWORDVAR =="AAA30 PASSWORD ASTEA",589,	
	ifelse (ETC$KEYWORDVAR =="AAB04 SAP TRAINING",590,	
	ifelse (ETC$KEYWORDVAR =="AAB12 SAP INVENTORY",591,	
	ifelse (ETC$KEYWORDVAR =="AAB14 SAP MATERIAL",592,	
	ifelse (ETC$KEYWORDVAR =="AAB19 SAP SHIP",593,	
	ifelse (ETC$KEYWORDVAR =="AAB21 SAP GROUP",594,	
	ifelse (ETC$KEYWORDVAR =="AAB22 SAP SETTING",595,	
	ifelse (ETC$KEYWORDVAR =="AAB24 SAP PART",596,	
	ifelse (ETC$KEYWORDVAR =="AAB25 SAP OUTPUT",597,	
	ifelse (ETC$KEYWORDVAR =="AAB26 SAP DOCUMENT",598,	
	ifelse (ETC$KEYWORDVAR =="AAB36 SAP BOM",599,	
	ifelse (ETC$KEYWORDVAR =="AAB39 SAP TICKET",600,	
	ifelse (ETC$KEYWORDVAR =="AAD05 VOICEMAIL ACCOUNT",601,	
	ifelse (ETC$KEYWORDVAR =="AAE06 EMAIL SERVER",602,	
	ifelse (ETC$KEYWORDVAR =="AAE15 EMAIL AUTH",603,	
	ifelse (ETC$KEYWORDVAR =="AAE21 EMAIL TOOL",604,	
	ifelse (ETC$KEYWORDVAR =="AAF16 MAIL GROUP",605,	
	ifelse (ETC$KEYWORDVAR =="AAF21 MAIL TOOL",606,	
	ifelse (ETC$KEYWORDVAR =="AAG04 PRINT LOGIN",607,	
	ifelse (ETC$KEYWORDVAR =="AAG11 PRINT AUTO",608,	
	ifelse (ETC$KEYWORDVAR =="AAG13 PRINT CANCEL",609,	
	ifelse (ETC$KEYWORDVAR =="AAG17 PRINT ZEBRA",610,	
	ifelse (ETC$KEYWORDVAR =="AAH01 LOGIN NOT WORK BW",611,	
	ifelse (ETC$KEYWORDVAR =="AAH12 LOGIN NOT WORK INTERNET",612,	
	ifelse (ETC$KEYWORDVAR =="AAH48 LOGIN ISSUE REMOTE",613,	
	ifelse (ETC$KEYWORDVAR =="AAK20 INSTALL PHONE",614,	
	ifelse (ETC$KEYWORDVAR =="AAK28 INSTALL SQL",615,	
	ifelse (ETC$KEYWORDVAR =="AAK32 INSTALL ISSUE",616,	
	ifelse (ETC$KEYWORDVAR =="AAL01 SETUP EPDM",617,	
	ifelse (ETC$KEYWORDVAR =="AAL04 SETUP WINDOWS",618,	
	ifelse (ETC$KEYWORDVAR =="AAL08 SETUP FAX",619,	
	ifelse (ETC$KEYWORDVAR =="AAL10 SETUP ADOBE",620,	
	ifelse (ETC$KEYWORDVAR =="AAL11 SETUP ALTIUM",621,	
	ifelse (ETC$KEYWORDVAR =="AAL14 SETUP CITRIX",622,	
	ifelse (ETC$KEYWORDVAR =="AAL16 SETUP DRIVE",623,	
	ifelse (ETC$KEYWORDVAR =="AAL17 SETUP PDF",624,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="AAL23 SETUP TOOL",625,	
	ifelse (ETC$KEYWORDVAR =="AAL24 SETUP NETWORK",626,	
	ifelse (ETC$KEYWORDVAR =="AAL25 SETUP INTERNATIONAL",627,	
	ifelse (ETC$KEYWORDVAR =="AAL28 SETUP SFDC",628,	
	ifelse (ETC$KEYWORDVAR =="AAL29 SETUP FOPS",629,	
	ifelse (ETC$KEYWORDVAR =="AAL30 SETUP SERVRER",630,	
	ifelse (ETC$KEYWORDVAR =="AAM02 CONFIG OFFICE",631,	
	ifelse (ETC$KEYWORDVAR =="AAM05 CONFIG PROJECT",632,	
	ifelse (ETC$KEYWORDVAR =="AAM06 CONFIG FIS",633,	
	ifelse (ETC$KEYWORDVAR =="AAM08 CONFIG ISSUE",634,	
	ifelse (ETC$KEYWORDVAR =="AAP06 SERVER VIRUS HACK",635,	
	ifelse (ETC$KEYWORDVAR =="AAQ11 NETWORK ACCOUNT",636,	
	ifelse (ETC$KEYWORDVAR =="AAR02 ROUTER DOWN NOTWORK",637,	
	ifelse (ETC$KEYWORDVAR =="AAR04 ROUTER ISSUE",638,	
	ifelse (ETC$KEYWORDVAR =="AAS02 SWITCH DOWN NOTWORK",639,	
	ifelse (ETC$KEYWORDVAR =="AAS04 SWITCH COMPUTER",640,	
	ifelse (ETC$KEYWORDVAR =="AAV03 STATION ACCOUNT",641,	
	ifelse (ETC$KEYWORDVAR =="AAW02 VM DOWN NOTWORK",642,	
	ifelse (ETC$KEYWORDVAR =="AAW05 VM COMPUTER",643,	
	ifelse (ETC$KEYWORDVAR =="AAZ02 JOB DOWN NOTWORK",644,	
	ifelse (ETC$KEYWORDVAR =="ABH02 EPDM DOWN NOTWORK",645,	
	ifelse (ETC$KEYWORDVAR =="ABK03 REFUSOL ISSUE",646,	
	ifelse (ETC$KEYWORDVAR =="ABN01 BOM ACESS CONNECT",647,	
	ifelse (ETC$KEYWORDVAR =="ABO01 FUSIONOPS ACESS CONNECT",648,	
	ifelse (ETC$KEYWORDVAR =="ABP03 ALTIUM ISSUE",649,	
	ifelse (ETC$KEYWORDVAR =="ABQ03 ENGINEER ISSUE",650,	
	ifelse (ETC$KEYWORDVAR =="ABS03 PROJECT ISSUE",651,	
	ifelse (ETC$KEYWORDVAR =="ABW99 ATS",652,	
	ifelse (ETC$KEYWORDVAR =="ABZ01 SALESFORCE ACESS CONNECT",653,	
	ifelse (ETC$KEYWORDVAR =="ACE02 EXCHANGE DOWN NOTWORK",654,	
	ifelse (ETC$KEYWORDVAR =="ACH02 DELL DOWN NOTWORK",655,	
	ifelse (ETC$KEYWORDVAR =="ACI03 CISCO ISSUE",656,	
	ifelse (ETC$KEYWORDVAR =="ACJ99 CITRIX",657,	
	ifelse (ETC$KEYWORDVAR =="ACK03 CAD ISSUE",658,	
	ifelse (ETC$KEYWORDVAR =="ACM03 SQL ISSUE",659,	
	ifelse (ETC$KEYWORDVAR =="ACT02 FOLDER DOWN NOTWORK",660,	
	ifelse (ETC$KEYWORDVAR =="ACT03 FOLDER ISSUE",661,	
	ifelse (ETC$KEYWORDVAR =="ACW01 SYSTEM UNATHOR ACCESS",662,	
	ifelse (ETC$KEYWORDVAR =="ACW10 SYSTEM ACCOUNT",663,	
	ifelse (ETC$KEYWORDVAR =="ACX07 SERVICE VIRUS HACK",664,	
	ifelse (ETC$KEYWORDVAR =="ACX11 SERVICE ADMIN",665,	
	ifelse (ETC$KEYWORDVAR =="ACZ05 NEW HIRE HARDWARE",666,	
	ifelse (ETC$KEYWORDVAR =="ACZ11 NEW HIRE CODE",667,	
	ifelse (ETC$KEYWORDVAR =="ACZ16 USERS DOWN NOTWORK",668,	
	ifelse (ETC$KEYWORDVAR =="ADA05 NEW SECURITY",669,	
	ifelse (ETC$KEYWORDVAR =="ADC02 COPIER DOWN NOTWORK",670,	
	ifelse (ETC$KEYWORDVAR =="ADE05 SCAN ADMIN",671,	
	ifelse (ETC$KEYWORDVAR =="ADS99 HARDWARE",672,	paste(ETC$KEYWORDVAR_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O1 <-	ifelse (ETC$KEYWORDVAR =="ADT01 INTERFACE ACESS CONNECT",673,	
	ifelse (ETC$KEYWORDVAR =="ADT02 INTERFACE DOWN NOTWORK",674,	
	ifelse (ETC$KEYWORDVAR =="ADT03 INTERFACE ISSUE",675,	
	ifelse (ETC$KEYWORDVAR =="ADW02 DOMAIN DOWN NOTWORK",676,	
	ifelse (ETC$KEYWORDVAR =="AEA02 LAN DOWN NOTWORK",677,	
	ifelse (ETC$KEYWORDVAR =="AEC99 IPAD IPHONE",678,	
	ifelse (ETC$KEYWORDVAR =="AEN01 SPACE NEW",679,	
	ifelse (ETC$KEYWORDVAR =="AEU01 PORT ACESS CONNECT",680,	
	ifelse (ETC$KEYWORDVAR =="AEU02 PORT DOWN NOTWORK",681,	
	ifelse (ETC$KEYWORDVAR =="AEU03 PORT ISSUE",682,	
	ifelse (ETC$KEYWORDVAR =="AEZ99 WARRANTY",683,	
	ifelse (ETC$KEYWORDVAR =="AGC99 MACHINE",684,	
	ifelse (ETC$KEYWORDVAR =="AGF03 DESKTOP ISSUE",685,	
	ifelse (ETC$KEYWORDVAR =="AGH01 TIME ACESS CONNECT",686,	
	ifelse (ETC$KEYWORDVAR =="AGI99 HACK",687,	
	ifelse (ETC$KEYWORDVAR =="AGJ99 ANONYMOUS",688,	
	ifelse (ETC$KEYWORDVAR =="AHB99 DEBUG",689,	
	ifelse (ETC$KEYWORDVAR =="AHC99 ASSET",690,	
	ifelse (ETC$KEYWORDVAR =="AHH99 FORMAT",691,	
	ifelse (ETC$KEYWORDVAR =="AHJ99 INVERT",692,	
	ifelse (ETC$KEYWORDVAR =="AHK99 ITEM",693,	
	ifelse (ETC$KEYWORDVAR =="AHL99 LABEL",694,	
	ifelse (ETC$KEYWORDVAR =="AHP99 PACK",695,	
	ifelse (ETC$KEYWORDVAR =="AIA99 SPYWARE",696,	
	ifelse (ETC$KEYWORDVAR =="AIC99 TRANSACTION",697,	
	ifelse (ETC$KEYWORDVAR =="AIF99 UPS",698,
	ifelse (ETC$KEYWORDVAR =="AIM99 CART",699,paste(ETC$KEYWORDVAR_O1))))))))))))))))))))))))))))
		

ETC$KEYWORDVAR_O1 <- ifelse (ETC$KEYWORDVAR_O1 == "NA", 415, paste(ETC$KEYWORDVAR_O1))

ETC$KEYWORDVAR_O2 <- "NA"

ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AAK21 INSTALL CITRIX",1,	
	ifelse (ETC$KEYWORDVAR =="ADE02 SCAN DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR =="ADY03 WIFI ISSUE",3,	
	ifelse (ETC$KEYWORDVAR =="AAV01 STATION ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR =="ACK99 CAD",5,	
	ifelse (ETC$KEYWORDVAR =="AAA28 PASSWORD VPN",6,	
	ifelse (ETC$KEYWORDVAR =="AAF09 MAIL SCAN",7,	
	ifelse (ETC$KEYWORDVAR =="AAP03 SERVER PATCH",8,	
	ifelse (ETC$KEYWORDVAR =="AAA25 PASSWORD RESET ADP",9,	
	ifelse (ETC$KEYWORDVAR =="AAA44 PASSWORD BW",10,	
	ifelse (ETC$KEYWORDVAR =="AAD02 VOICEMAIL ACESS CONNECT",11,	
	ifelse (ETC$KEYWORDVAR =="AAG08 NEW PRINTER",12,	
	ifelse (ETC$KEYWORDVAR =="AAH26 LOGIN LONG",13,	
	ifelse (ETC$KEYWORDVAR =="AAL06 SETUP PROJECT",14,	
	ifelse (ETC$KEYWORDVAR =="AAM03 CONFIG ADMIN",15,	
	ifelse (ETC$KEYWORDVAR =="ADV01 PORTAL ACESS CONNECT",16,	
	ifelse (ETC$KEYWORDVAR =="AFF99 BILL",17,	
	ifelse (ETC$KEYWORDVAR =="AAF04 MAIL LOGIN",18,	
	ifelse (ETC$KEYWORDVAR =="AAA46 PASSWORD AE",19,	
	ifelse (ETC$KEYWORDVAR =="ABR02 SHAREPOINT DOWN NOTWORK",20,	
	ifelse (ETC$KEYWORDVAR =="AAA13 PASSWORD RESET WINDOWS",21,	
	ifelse (ETC$KEYWORDVAR =="AAH43 LOGIN ISSUE LAPTOP",22,	
	ifelse (ETC$KEYWORDVAR =="ABN03 BOM ISSUE",23,	
	ifelse (ETC$KEYWORDVAR =="ADE10 SCAN ISSUE",24,	
	ifelse (ETC$KEYWORDVAR =="AAH08 LOGIN NOT WORK EXCHANGE",25,	
	ifelse (ETC$KEYWORDVAR =="AAA39 PASSWORD RESET TS3",26,	
	ifelse (ETC$KEYWORDVAR =="AAH25 LOGIN ACCESS CONNECT",27,	
	ifelse (ETC$KEYWORDVAR =="AAH31 LOGIN ISSUE BW",28,	
	ifelse (ETC$KEYWORDVAR =="ADK99 ACROBAT",29,	
	ifelse (ETC$KEYWORDVAR =="AAL15 SETUP DELL",30,	
	ifelse (ETC$KEYWORDVAR =="AHS99 PRODUCT",31,	
	ifelse (ETC$KEYWORDVAR =="ADD04 FAX COMPUTER",32,	
	ifelse (ETC$KEYWORDVAR =="AAA01 PASSWORD RESET EMAIL",33,	
	ifelse (ETC$KEYWORDVAR =="AAA20 PASSWORD ACCOUNT",34,	
	ifelse (ETC$KEYWORDVAR =="AAA35 PASSWORD RESET PCARD",35,	
	ifelse (ETC$KEYWORDVAR =="AAD04 VOICEMAIL LOGIN",36,	
	ifelse (ETC$KEYWORDVAR =="AAH36 LOGIN ISSUE EMPLOYEE",37,	
	ifelse (ETC$KEYWORDVAR =="AAU05 LINK ISSUE",38,	
	ifelse (ETC$KEYWORDVAR =="ABS02 PROJECT DOWN NOTWORK",39,	
	ifelse (ETC$KEYWORDVAR =="AAA05 PASSWORD RESET NETWORK",40,	
	ifelse (ETC$KEYWORDVAR =="AAA40 PASSWORD TS3",41,	
	ifelse (ETC$KEYWORDVAR =="AAG16 PRINT INVOICE",42,	
	ifelse (ETC$KEYWORDVAR =="AAH05 LOGIN NOT WORK DOMAIN",43,	
	ifelse (ETC$KEYWORDVAR =="AAA47 PASSWORD RESET AUTO REPLY",44,	
	ifelse (ETC$KEYWORDVAR =="AAL22 SETUP STATION",45,	
	ifelse (ETC$KEYWORDVAR =="ADX03 WIRELESS ISSUE",46,	
	ifelse (ETC$KEYWORDVAR =="AAA45 PASSWORD RESET AE",47,	
	ifelse (ETC$KEYWORDVAR =="AAL20 SETUP WIRELESS",48,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AAP10 SERVER ADMIN",49,	
	ifelse (ETC$KEYWORDVAR =="AGY99 DONGLE",50,	
	ifelse (ETC$KEYWORDVAR =="AIB99 TERMINATION",51,	
	ifelse (ETC$KEYWORDVAR =="AAA33 PASSWORD RESET ITCHECK",52,	
	ifelse (ETC$KEYWORDVAR =="AAF26 MAIL ISSUE",53,	
	ifelse (ETC$KEYWORDVAR =="ABZ02 SALESFORCE DOWN NOTWORK",54,	
	ifelse (ETC$KEYWORDVAR =="AAA29 PASSWORD RESET ASTEA",55,	
	ifelse (ETC$KEYWORDVAR =="AAA11 PASSWORD RESET COMPUTER",56,	
	ifelse (ETC$KEYWORDVAR =="ABO99 FUSIONOPS",57,	
	ifelse (ETC$KEYWORDVAR =="AFB99 PURCHASE",58,	
	ifelse (ETC$KEYWORDVAR =="AIR99 TEST",59,	
	ifelse (ETC$KEYWORDVAR =="AAF10 MAIL PHONE",60,	
	ifelse (ETC$KEYWORDVAR =="AAH04 LOGIN NOT WORK DELL",61,	
	ifelse (ETC$KEYWORDVAR =="AAA03 PASSWORD RESET EXCHANGE",62,	
	ifelse (ETC$KEYWORDVAR =="AAA24 PASSWORD USER",63,	
	ifelse (ETC$KEYWORDVAR =="ABI03 PLM ISSUE",64,	
	ifelse (ETC$KEYWORDVAR =="ACA99 MS OFFICE",65,	
	ifelse (ETC$KEYWORDVAR =="AAA19 PASSWORD RESET ACCOUNT",66,	
	ifelse (ETC$KEYWORDVAR =="ABM02 BW DOWN NOTWORK",67,	
	ifelse (ETC$KEYWORDVAR =="ABM03 BW ISSUE",68,	
	ifelse (ETC$KEYWORDVAR =="ACB02 OFFICE DOWN NOTWORK",69,	
	ifelse (ETC$KEYWORDVAR =="AAB37 SAP BW",70,	
	ifelse (ETC$KEYWORDVAR =="AAF07 MAIL ACCOUNT",71,	
	ifelse (ETC$KEYWORDVAR =="AAH53 LOGIN ISSUE VPN",72,	
	ifelse (ETC$KEYWORDVAR =="ABL02 ASTEA DOWN NOTWORK",73,	
	ifelse (ETC$KEYWORDVAR =="AEP99 PAYROLL",74,	
	ifelse (ETC$KEYWORDVAR =="ABZ03 SALESFORCE ISSUE",75,	
	ifelse (ETC$KEYWORDVAR =="ACD02 OUTLOOK DOWN NOTWORK",76,	
	ifelse (ETC$KEYWORDVAR =="ACW07 SYSTEM VIRUS HACK",77,	
	ifelse (ETC$KEYWORDVAR =="ADM99 FLASH",78,	
	ifelse (ETC$KEYWORDVAR =="AGD01 COMPUTER ACESS CONNECT",79,	
	ifelse (ETC$KEYWORDVAR =="AHQ99 PROCESS",80,	
	ifelse (ETC$KEYWORDVAR =="AIQ99 TRAINING",81,	
	ifelse (ETC$KEYWORDVAR =="AAA02 PASSWORD EMAIL",82,	
	ifelse (ETC$KEYWORDVAR =="AAJ07 LOCK PHONE",83,	
	ifelse (ETC$KEYWORDVAR =="AAK16 INSTALL CAD",84,	
	ifelse (ETC$KEYWORDVAR =="ACF01 INTRANET ACESS CONNECT",85,	
	ifelse (ETC$KEYWORDVAR =="ACW11 SYSTEM ADMIN",86,	
	ifelse (ETC$KEYWORDVAR =="ADY02 WIFI DOWN NOTWORK",87,	
	ifelse (ETC$KEYWORDVAR =="AAA27 PASSWORD RESET VPN",88,	
	ifelse (ETC$KEYWORDVAR =="AAC04 WEBEX LOGIN",89,	
	ifelse (ETC$KEYWORDVAR =="AAF01 MAIL DELIVERY",90,	
	ifelse (ETC$KEYWORDVAR =="AAK04 INSTALL VPN",91,	
	ifelse (ETC$KEYWORDVAR =="AAM07 CONFIG COMPUTER",92,	
	ifelse (ETC$KEYWORDVAR =="ABJ02 TS3 DOWN NOTWORK",93,	
	ifelse (ETC$KEYWORDVAR =="ACQ99 MCAFEE",94,	
	ifelse (ETC$KEYWORDVAR =="ADQ99 REPORT",95,	
	ifelse (ETC$KEYWORDVAR =="AHW99 RESTART",96,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AAA16 PASSWORD PHONE",97,	
	ifelse (ETC$KEYWORDVAR =="AAB35 SAP TS3",98,	
	ifelse (ETC$KEYWORDVAR =="AGG02 PC DOWN NOTWORK",99,	
	ifelse (ETC$KEYWORDVAR =="AAA09 PASSWORD RESET WEBEX",100,	
	ifelse (ETC$KEYWORDVAR =="AAC09 WEBEX AUTH",101,	
	ifelse (ETC$KEYWORDVAR =="AAQ14 NETWORK ISSUE",102,	
	ifelse (ETC$KEYWORDVAR =="AGW99 QUICKEN",103,	
	ifelse (ETC$KEYWORDVAR =="AAC16 WEBEX REQUEST",104,	
	ifelse (ETC$KEYWORDVAR =="ADC01 COPIER ACESS CONNECT",105,	
	ifelse (ETC$KEYWORDVAR =="AHY99 SCREEN",106,	
	ifelse (ETC$KEYWORDVAR =="AAB18 SAP RESET",107,	
	ifelse (ETC$KEYWORDVAR =="AAB32 SAP PORTAL",108,	
	ifelse (ETC$KEYWORDVAR =="AAB33 SAP REFUSOL",109,	
	ifelse (ETC$KEYWORDVAR =="ABE01 TOOL DOWN NOTWORK",110,	
	ifelse (ETC$KEYWORDVAR =="AAE09 EMAIL SCAN",111,	
	ifelse (ETC$KEYWORDVAR =="ADE99 SCAN",112,	
	ifelse (ETC$KEYWORDVAR =="AAW99 VM",113,	
	ifelse (ETC$KEYWORDVAR =="ADX01 WIRELESS ACESS CONNECT",114,	
	ifelse (ETC$KEYWORDVAR =="AAL13 SETUP CISCO",115,	
	ifelse (ETC$KEYWORDVAR =="ABU99 ADOBE",116,	
	ifelse (ETC$KEYWORDVAR =="ADB08 PHONE CHANGE",117,	
	ifelse (ETC$KEYWORDVAR =="ADY99 WIFI",118,	
	ifelse (ETC$KEYWORDVAR =="AFQ99 EFT",119,	
	ifelse (ETC$KEYWORDVAR =="AIG99 WEB",120,	
	ifelse (ETC$KEYWORDVAR =="AAC02 WEBEX ACESS",121,	
	ifelse (ETC$KEYWORDVAR =="AAE14 EMAIL NOTIFICATION",122,	
	ifelse (ETC$KEYWORDVAR =="AAK08 INSTALL VISIO",123,	
	ifelse (ETC$KEYWORDVAR =="ABV99 GOOGLE",124,	
	ifelse (ETC$KEYWORDVAR =="ACC01 EXCEL ACESS CONNECT",125,	
	ifelse (ETC$KEYWORDVAR =="ADG01 CRITICAL ALERT",126,	
	ifelse (ETC$KEYWORDVAR =="ADZ99 REMOTE",127,	
	ifelse (ETC$KEYWORDVAR =="AAA49 PASSWORD RESET",128,	
	ifelse (ETC$KEYWORDVAR =="AAH27 NEW LOGIN",129,	
	ifelse (ETC$KEYWORDVAR =="AAH15 LOGIN NOT WORK OUTLOOK",130,	
	ifelse (ETC$KEYWORDVAR =="AAK12 INSTALL PROJECT",131,	
	ifelse (ETC$KEYWORDVAR =="ACG99 EXTRANET",132,	
	ifelse (ETC$KEYWORDVAR =="AHV99 RESET",133,	
	ifelse (ETC$KEYWORDVAR =="AAC08 WEBEX CHANGE",134,	
	ifelse (ETC$KEYWORDVAR =="AAJ10 LOCK ACCOUNT",135,	
	ifelse (ETC$KEYWORDVAR =="AAA08 PASSWORD SAP",136,	
	ifelse (ETC$KEYWORDVAR =="AAH20 LOGIN NOT WORK SFDC",137,	
	ifelse (ETC$KEYWORDVAR =="AAQ99 NETWORK",138,	
	ifelse (ETC$KEYWORDVAR =="ADZ01 REMOTE ACESS CONNECT",139,	
	ifelse (ETC$KEYWORDVAR =="ACP01 SITE ACESS CONNECT",140,	
	ifelse (ETC$KEYWORDVAR =="AGF02 DESKTOP DOWN NOTWORK",141,	
	ifelse (ETC$KEYWORDVAR =="ACH01 DELL ACESS CONNECT",142,	
	ifelse (ETC$KEYWORDVAR =="AAO99 VPN",143,	
	ifelse (ETC$KEYWORDVAR =="AAP11 SERVER MAINTENANCE",144,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="ADB09 PHONE DELIVERY",145,	
	ifelse (ETC$KEYWORDVAR =="AAB02 SAP DOWN NOTWORK",146,	
	ifelse (ETC$KEYWORDVAR =="AAX03 EQUIPMENT ISSUE",147,	
	ifelse (ETC$KEYWORDVAR =="ACZ17 USERS ISSUE",148,	
	ifelse (ETC$KEYWORDVAR =="AAN03 INTERNET ISSUE",149,	
	ifelse (ETC$KEYWORDVAR =="AAA07 PASSWORD RESET SAP",150,	
	ifelse (ETC$KEYWORDVAR =="AAA99 PASSWORD",151,	
	ifelse (ETC$KEYWORDVAR =="AAL12 SETUP PHONE",152,	
	ifelse (ETC$KEYWORDVAR =="ADP99 USB",153,	
	ifelse (ETC$KEYWORDVAR =="AHN99 MESSAGE",154,	
	ifelse (ETC$KEYWORDVAR =="AAA36 PASSWORD PCARD",155,	
	ifelse (ETC$KEYWORDVAR =="AAE25 EMAIL ISSUE",156,	
	ifelse (ETC$KEYWORDVAR =="AAK27 INSTALL SKYPE",157,	
	ifelse (ETC$KEYWORDVAR =="AAK30 INSTALL IE",158,	
	ifelse (ETC$KEYWORDVAR =="AAQ10 NETWORK COMPUTER",159,	
	ifelse (ETC$KEYWORDVAR =="ACF99 INTRANET",160,	
	ifelse (ETC$KEYWORDVAR =="ACI01 CISCO ACESS CONNECT",161,	
	ifelse (ETC$KEYWORDVAR =="AFK99 GROUP",162,	
	ifelse (ETC$KEYWORDVAR =="AHI99 GEAR",163,	
	ifelse (ETC$KEYWORDVAR =="ACB03 OFFICE ISSUE",164,	
	ifelse (ETC$KEYWORDVAR =="ACC03 EXCEL ISSUE",165,	
	ifelse (ETC$KEYWORDVAR =="ADW99 DOMAIN",166,	
	ifelse (ETC$KEYWORDVAR =="AAC01 WEBEX DISABLE",167,	
	ifelse (ETC$KEYWORDVAR =="AAC06 WEBEX ACCOUNT",168,	
	ifelse (ETC$KEYWORDVAR =="AEJ99 HEADSET",169,	
	ifelse (ETC$KEYWORDVAR =="AAG02 PRINT DOWN NOTWORK PROD",170,	
	ifelse (ETC$KEYWORDVAR =="AAO02 VPN DOWN NOTWORK",171,	
	ifelse (ETC$KEYWORDVAR =="AAF02 MAIL ACCESS CONNECT",172,	
	ifelse (ETC$KEYWORDVAR =="AAK03 INSTALL FIS",173,	
	ifelse (ETC$KEYWORDVAR =="AAL02 SETUP VPN",174,	
	ifelse (ETC$KEYWORDVAR =="AAA10 PASSWORD WEBEX",175,	
	ifelse (ETC$KEYWORDVAR =="AAC14 WEBEX TOOL",176,	
	ifelse (ETC$KEYWORDVAR =="AAJ03 LOCK CISCO",177,	
	ifelse (ETC$KEYWORDVAR =="ADG99 CRITICAL",178,	
	ifelse (ETC$KEYWORDVAR =="AHF99 DIRECTORY",179,	
	ifelse (ETC$KEYWORDVAR =="AAK25 INSTALL EXCEL",180,	
	ifelse (ETC$KEYWORDVAR =="ADB11 PHONE DIRECTORY",181,	
	ifelse (ETC$KEYWORDVAR =="AGM99 SPAM",182,	
	ifelse (ETC$KEYWORDVAR =="AHG99 FIRM",183,	
	ifelse (ETC$KEYWORDVAR =="ADD99 FAX",184,	
	ifelse (ETC$KEYWORDVAR =="AEA01 LAN ACESS CONNECT",185,	
	ifelse (ETC$KEYWORDVAR =="AGG01 PC ACESS CONNECT",186,	
	ifelse (ETC$KEYWORDVAR =="AAA43 PASSWORD RESET BW",187,	
	ifelse (ETC$KEYWORDVAR =="AAB38 SAP TOOL",188,	
	ifelse (ETC$KEYWORDVAR =="AAF03 MAIL DOWN NOTWORK",189,	
	ifelse (ETC$KEYWORDVAR =="ABI99 PLM",190,	
	ifelse (ETC$KEYWORDVAR =="AAC12 WEBEX MEETING",191,	
	ifelse (ETC$KEYWORDVAR =="ACP02 SITE DOWN NOTWORK",192,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AGB99 POWER",193,	
	ifelse (ETC$KEYWORDVAR =="AAK29 INSTALL EXPENS",194,	
	ifelse (ETC$KEYWORDVAR =="ABA02 WINZIP DOWN NOTWORK",195,	
	ifelse (ETC$KEYWORDVAR =="ACB01 OFFICE ACESS CONNECT",196,	
	ifelse (ETC$KEYWORDVAR =="ADR99 SOFTWARE",197,	
	ifelse (ETC$KEYWORDVAR =="AAH23 LOGIN NOT WORK VPN",198,	
	ifelse (ETC$KEYWORDVAR =="AAK26 INSTALL PDF",199,	
	ifelse (ETC$KEYWORDVAR =="ACX10 SERVICE ACCOUNT",200,	
	ifelse (ETC$KEYWORDVAR =="AGH02 TIME DOWN NOTWORK",201,	
	ifelse (ETC$KEYWORDVAR =="AGZ99 DATABASE",202,	
	ifelse (ETC$KEYWORDVAR =="AAB10 SAP INSTALL",203,	
	ifelse (ETC$KEYWORDVAR =="AAC15 WEBEX ISSUE",204,	
	ifelse (ETC$KEYWORDVAR =="AAW01 VM ACESS CONNECT",205,	
	ifelse (ETC$KEYWORDVAR =="AHR99 PRODUCTION",206,	
	ifelse (ETC$KEYWORDVAR =="AAK14 INSTALL FAX",207,	
	ifelse (ETC$KEYWORDVAR =="ACZ13 NEW HIRE COMPUTER",208,	
	ifelse (ETC$KEYWORDVAR =="AEB99 BOX",209,	
	ifelse (ETC$KEYWORDVAR =="ABS01 PROJECT ACESS CONNECT",210,	
	ifelse (ETC$KEYWORDVAR =="AGD03 COMPUTER ISSUE",211,	
	ifelse (ETC$KEYWORDVAR =="AAC05 WEBEX INSTALL",212,	
	ifelse (ETC$KEYWORDVAR =="AAK02 INSTALL EPDM",213,	
	ifelse (ETC$KEYWORDVAR =="AAK22 INSTALL DELL",214,	
	ifelse (ETC$KEYWORDVAR =="ACV03 FILE ISSUE",215,	
	ifelse (ETC$KEYWORDVAR =="AFG99 DELIVER",216,	
	ifelse (ETC$KEYWORDVAR =="AAB03 SAP LOGIN",217,	
	ifelse (ETC$KEYWORDVAR =="AAB13 SAP ITEM",218,	
	ifelse (ETC$KEYWORDVAR =="AAH30 LOGIN ISSUE",219,	
	ifelse (ETC$KEYWORDVAR =="AAH34 LOGIN ISSUE DELL",220,	
	ifelse (ETC$KEYWORDVAR =="ACO99 WEBSITE",221,	
	ifelse (ETC$KEYWORDVAR =="AAD01 VOICEMAIL DISABLE",222,	
	ifelse (ETC$KEYWORDVAR =="AAJ99 LOCK",223,	
	ifelse (ETC$KEYWORDVAR =="ABA99 WINZIP",224,	
	ifelse (ETC$KEYWORDVAR =="ADN99 CPU",225,	
	ifelse (ETC$KEYWORDVAR =="AAC99 WEBEX",226,	
	ifelse (ETC$KEYWORDVAR =="AAT99 NODE",227,	
	ifelse (ETC$KEYWORDVAR =="ACM01 SQL ACESS CONNECT",228,	
	ifelse (ETC$KEYWORDVAR =="ACO02 WEBSITE DOWN NOTWORK",229,	
	ifelse (ETC$KEYWORDVAR =="ABH01 EPDM ACESS CONNECT",230,	
	ifelse (ETC$KEYWORDVAR =="ACD03 OUTLOOK ISSUE",231,	
	ifelse (ETC$KEYWORDVAR =="ACH99 DELL",232,	
	ifelse (ETC$KEYWORDVAR =="AAE12 EMAIL CHANGE",233,	
	ifelse (ETC$KEYWORDVAR =="ABT99 PDF",234,	
	ifelse (ETC$KEYWORDVAR =="ACI99 CISCO",235,	
	ifelse (ETC$KEYWORDVAR =="AGU99 HELP DESK",236,	
	ifelse (ETC$KEYWORDVAR =="AAA48 PASSWORD RESET AUTO",237,	
	ifelse (ETC$KEYWORDVAR =="AAD99 VOICEMAIL",238,	
	ifelse (ETC$KEYWORDVAR =="ABR99 SHAREPOINT",239,	
	ifelse (ETC$KEYWORDVAR =="AHX99 RESOURCE",240,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AAV02 STATION DOWN NOTWORK",241,	
	ifelse (ETC$KEYWORDVAR =="ACZ14 NEW HIRE",242,	
	ifelse (ETC$KEYWORDVAR =="AAF05 MAIL INSTALL",243,	
	ifelse (ETC$KEYWORDVAR =="ACO01 WEBSITE ACESS CONNECT",244,	
	ifelse (ETC$KEYWORDVAR =="ACV01 FILE ACESS CONNECT",245,	
	ifelse (ETC$KEYWORDVAR =="ACX02 SERVICE ACESS CONNECT",246,	
	ifelse (ETC$KEYWORDVAR =="AFL99 CASE",247,	
	ifelse (ETC$KEYWORDVAR =="AFW99 CONFERENCE",248,	
	ifelse (ETC$KEYWORDVAR =="ADY01 WIFI ACESS CONNECT",249,	
	ifelse (ETC$KEYWORDVAR =="AHT99 RECORD",250,	
	ifelse (ETC$KEYWORDVAR =="AAA12 PASSWORD COMPUTER",251,	
	ifelse (ETC$KEYWORDVAR =="ACU99 DRIVE",252,	
	ifelse (ETC$KEYWORDVAR =="AGO99 DOWN",253,	
	ifelse (ETC$KEYWORDVAR =="ADB05 PHONE COMPUTER",254,	
	ifelse (ETC$KEYWORDVAR =="AIH99 TICKET",255,	
	ifelse (ETC$KEYWORDVAR =="AAL19 SETUP EXPENS",256,	
	ifelse (ETC$KEYWORDVAR =="AAY99 INVENTORY",257,	
	ifelse (ETC$KEYWORDVAR =="ADW01 DOMAIN ACESS CONNECT",258,	
	ifelse (ETC$KEYWORDVAR =="AAC17 WEBEX CONNECT",259,	
	ifelse (ETC$KEYWORDVAR =="AAL27 SETUP TS3",260,	
	ifelse (ETC$KEYWORDVAR =="AAP12 SERVER ISSUE",261,	
	ifelse (ETC$KEYWORDVAR =="ADA02 NEW GENERAL",262,	
	ifelse (ETC$KEYWORDVAR =="ADB12 PHONE ISSUE",263,	
	ifelse (ETC$KEYWORDVAR =="AFE99 TAX",264,	
	ifelse (ETC$KEYWORDVAR =="AAH28 LOGIN CHANGE",265,	
	ifelse (ETC$KEYWORDVAR =="AAQ01 NETWORK WIRELESS",266,	
	ifelse (ETC$KEYWORDVAR =="ABE99 TOOL",267,	
	ifelse (ETC$KEYWORDVAR =="AGD02 COMPUTER DOWN NOTWORK",268,	
	ifelse (ETC$KEYWORDVAR =="AAC11 WEBEX OUTLOOK",269,	
	ifelse (ETC$KEYWORDVAR =="AAN99 INTERNET",270,	
	ifelse (ETC$KEYWORDVAR =="AAV99 STATION",271,	
	ifelse (ETC$KEYWORDVAR =="AAF20 MAIL OUTLOOK",272,	
	ifelse (ETC$KEYWORDVAR =="AAK09 INSTALL GOOGLE",273,	
	ifelse (ETC$KEYWORDVAR =="ABF02 SFDC ISSUE",274,	
	ifelse (ETC$KEYWORDVAR =="ACY10 APP ACCOUNT",275,	
	ifelse (ETC$KEYWORDVAR =="AAK07 INSTALL WINDOWS",276,	
	ifelse (ETC$KEYWORDVAR =="AFV99 MEET",277,	
	ifelse (ETC$KEYWORDVAR =="AAA23 PASSWORD RESET USER",278,	
	ifelse (ETC$KEYWORDVAR =="AAQ03 NETWORK DOWN NOTWORK",279,	
	ifelse (ETC$KEYWORDVAR =="ABD02 PTOOL ISSUE",280,	
	ifelse (ETC$KEYWORDVAR =="AAG99 PRINT",281,	
	ifelse (ETC$KEYWORDVAR =="ABQ99 ENGINEER",282,	
	ifelse (ETC$KEYWORDVAR =="ACZ06 NEW HIRE PHONE",283,	
	ifelse (ETC$KEYWORDVAR =="AAH24 LOGIN NOT WORK",284,	
	ifelse (ETC$KEYWORDVAR =="ABR03 SHAREPOINT ISSUE",285,	
	ifelse (ETC$KEYWORDVAR =="AAO01 VPN ACESS CONNECT",286,	
	ifelse (ETC$KEYWORDVAR =="AAT02 NODE DOWN NOTWORK",287,	
	ifelse (ETC$KEYWORDVAR =="AFA99 INVOICE",288,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AAJ04 LOCK COMPUTER",289,	
	ifelse (ETC$KEYWORDVAR =="AAK18 INSTALL ADOBE",290,	
	ifelse (ETC$KEYWORDVAR =="AAE99 EMAIL",291,	
	ifelse (ETC$KEYWORDVAR =="AAS99 SWITCH",292,	
	ifelse (ETC$KEYWORDVAR =="AFM99 LIST",293,	
	ifelse (ETC$KEYWORDVAR =="AAC10 WEBEX SETTING",294,	
	ifelse (ETC$KEYWORDVAR =="AAG12 PRINT CHANGE",295,	
	ifelse (ETC$KEYWORDVAR =="AEU99 PORT",296,	
	ifelse (ETC$KEYWORDVAR =="AGP99 ACCESS",297,	
	ifelse (ETC$KEYWORDVAR =="AAL99 SETUP",298,	
	ifelse (ETC$KEYWORDVAR =="AAU02 LINK DOWN NOTWORK",299,	
	ifelse (ETC$KEYWORDVAR =="AAM01 CONFIG OUTLOOK",300,	
	ifelse (ETC$KEYWORDVAR =="AAZ01 JOB STEP1",301,	
	ifelse (ETC$KEYWORDVAR =="AGG99 PC",302,	
	ifelse (ETC$KEYWORDVAR =="AAE08 EMAIL PRINT",303,	
	ifelse (ETC$KEYWORDVAR =="AAO03 VPN ISSUE",304,	
	ifelse (ETC$KEYWORDVAR =="ACM99 SQL",305,	
	ifelse (ETC$KEYWORDVAR =="AEM99 MEMORY",306,	
	ifelse (ETC$KEYWORDVAR =="AAF99 MAIL",307,	
	ifelse (ETC$KEYWORDVAR =="AAQ02 NETWORK ACESS CONNECT",308,	
	ifelse (ETC$KEYWORDVAR =="ABQ01 ENGINEER ACESS CONNECT",309,	
	ifelse (ETC$KEYWORDVAR =="ADH99 REQ",310,	
	ifelse (ETC$KEYWORDVAR =="AAK01 INSTALL PLM",311,	
	ifelse (ETC$KEYWORDVAR =="AAH07 LOGIN NOT WORK EPMD",312,	
	ifelse (ETC$KEYWORDVAR =="AAI99 BLOCK",313,	
	ifelse (ETC$KEYWORDVAR =="AAL26 SETUP TECHNICIAN",314,	
	ifelse (ETC$KEYWORDVAR =="AAN02 INTERNET DOWN NOTWORK",315,	
	ifelse (ETC$KEYWORDVAR =="AAE16 EMAIL GROUP",316,	
	ifelse (ETC$KEYWORDVAR =="ADF01 INCIDENT ATTACK",317,	
	ifelse (ETC$KEYWORDVAR =="AAN01 INTERNET ACESS CONNECT",318,	
	ifelse (ETC$KEYWORDVAR =="AAH17 LOGIN NOT WORK PORTAL",319,	
	ifelse (ETC$KEYWORDVAR =="AAH52 LOGIN ISSUE TOOL",320,	
	ifelse (ETC$KEYWORDVAR =="AAQ08 NETWORK FILE",321,	
	ifelse (ETC$KEYWORDVAR =="ABC99 WIN",322,	
	ifelse (ETC$KEYWORDVAR =="AIP99 IE",323,	
	ifelse (ETC$KEYWORDVAR =="ACW13 SYSTEM ISSUE",324,	
	ifelse (ETC$KEYWORDVAR =="AFO99 BACKUP",325,	
	ifelse (ETC$KEYWORDVAR =="AAE02 EMAIL ACCESS CONNECT",326,	
	ifelse (ETC$KEYWORDVAR =="AAG20 PRINT ISSUE",327,	
	ifelse (ETC$KEYWORDVAR =="AAK24 INSTALL ENGINEER",328,	
	ifelse (ETC$KEYWORDVAR =="ADA06 NEW PHONE",329,	
	ifelse (ETC$KEYWORDVAR =="ADA07 NEW COMPUTER",330,	
	ifelse (ETC$KEYWORDVAR =="ACK01 CAD ACESS CONNECT",331,	
	ifelse (ETC$KEYWORDVAR =="AEQ99 PTO",332,	
	ifelse (ETC$KEYWORDVAR =="AGS99 CHANGE",333,	
	ifelse (ETC$KEYWORDVAR =="AAG03 PRINT DOWN NOTWORK",334,	
	ifelse (ETC$KEYWORDVAR =="AAL21 SETUP TABLET",335,	
	ifelse (ETC$KEYWORDVAR =="ADV02 PORTAL DOWN NOTWORK",336,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AEG99 KEYBOARD",337,	
	ifelse (ETC$KEYWORDVAR =="AGD99 COMPUTER",338,	
	ifelse (ETC$KEYWORDVAR =="AAE05 EMAIL INSTALL",339,	
	ifelse (ETC$KEYWORDVAR =="AAH99 LOGIN",340,	
	ifelse (ETC$KEYWORDVAR =="AAU99 LINK",341,	
	ifelse (ETC$KEYWORDVAR =="ABM99 BW",342,	
	ifelse (ETC$KEYWORDVAR =="ACZ18 USERS",343,	
	ifelse (ETC$KEYWORDVAR =="AGL99 MALWARE",344,	
	ifelse (ETC$KEYWORDVAR =="AGQ99 ADMIN",345,	
	ifelse (ETC$KEYWORDVAR =="AAE03 EMAIL DOWN NOTWORK",346,	
	ifelse (ETC$KEYWORDVAR =="ADA99 NEW",347,	
	ifelse (ETC$KEYWORDVAR =="AFU99 INFO",348,	
	ifelse (ETC$KEYWORDVAR =="AAB01 SAP ACESS CONNECT",349,	
	ifelse (ETC$KEYWORDVAR =="ACA02 MS OFFICE DOWN NOTWORK",350,	
	ifelse (ETC$KEYWORDVAR =="ABD99 PTOOL",351,	
	ifelse (ETC$KEYWORDVAR =="AAA15 PASSWORD RESET PHONE",352,	
	ifelse (ETC$KEYWORDVAR =="AAL07 SETUP SCAN",353,	
	ifelse (ETC$KEYWORDVAR =="ABJ99 TS3",354,	
	ifelse (ETC$KEYWORDVAR =="ACD99 OUTLOOK",355,	
	ifelse (ETC$KEYWORDVAR =="ACV99 FILE",356,	
	ifelse (ETC$KEYWORDVAR =="ADB99 PHONE",357,	
	ifelse (ETC$KEYWORDVAR =="AAE20 EMAIL OUTLOOK",358,	
	ifelse (ETC$KEYWORDVAR =="AAH33 LOGIN ISSUE COMPUTER",359,	
	ifelse (ETC$KEYWORDVAR =="ABK01 REFUSOL ACESS CONNECT",360,	
	ifelse (ETC$KEYWORDVAR =="ADA04 NEW SOFTWARE",361,	
	ifelse (ETC$KEYWORDVAR =="AET99 EXPENSE",362,	
	ifelse (ETC$KEYWORDVAR =="ACU01 DRIVE ACESS CONNECT",363,	
	ifelse (ETC$KEYWORDVAR =="AFT99 DOC",364,	
	ifelse (ETC$KEYWORDVAR =="ACD01 OUTLOOK ACESS CONNECT",365,	
	ifelse (ETC$KEYWORDVAR =="AAC03 WEBEX DOWN NOTWORK",366,	
	ifelse (ETC$KEYWORDVAR =="ABZ99 SALESFORCE",367,	
	ifelse (ETC$KEYWORDVAR =="ACY02 APP ACESS CONNECT",368,	
	ifelse (ETC$KEYWORDVAR =="AFY99 CALENDAR",369,	
	ifelse (ETC$KEYWORDVAR =="AAF06 MAIL SERVER",370,	
	ifelse (ETC$KEYWORDVAR =="AAH13 LOGIN NOT WORK LAPTOP",371,	
	ifelse (ETC$KEYWORDVAR =="ACZ12 NEW HIRE ACCOUNT",372,	
	ifelse (ETC$KEYWORDVAR =="ACB99 OFFICE",373,	
	ifelse (ETC$KEYWORDVAR =="ACZ99 USER",374,	
	ifelse (ETC$KEYWORDVAR =="AAQ05 NETWORK BACKUP",375,	
	ifelse (ETC$KEYWORDVAR =="AGH99 TIME",376,	
	ifelse (ETC$KEYWORDVAR =="AAX99 EQUIPMENT",377,	
	ifelse (ETC$KEYWORDVAR =="ABB99 WINDOWS",378,	
	ifelse (ETC$KEYWORDVAR =="AAB40 SAP ISSUE",379,	
	ifelse (ETC$KEYWORDVAR =="AAA37 PASSWORD RESET PC",380,	
	ifelse (ETC$KEYWORDVAR =="ACH03 DELL ISSUE",381,	
	ifelse (ETC$KEYWORDVAR =="ACY99 APP",382,	
	ifelse (ETC$KEYWORDVAR =="OTHER",383,	
	ifelse (ETC$KEYWORDVAR =="AAE01 EMAIL DELIVERY",384,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="ACZ21 USER ISSUE",385,	
	ifelse (ETC$KEYWORDVAR =="AAB08 SAP ACCOUNT",386,	
	ifelse (ETC$KEYWORDVAR =="AAB99 SAP",387,	
	ifelse (ETC$KEYWORDVAR =="AAV06 STATION ISSUE",388,	
	ifelse (ETC$KEYWORDVAR =="ADV99 PORTAL",389,	
	ifelse (ETC$KEYWORDVAR =="ABD01 PTOOL DOWN NOTWORK",390,	
	ifelse (ETC$KEYWORDVAR =="AAP01 SERVER ACESS CONNECT",391,	
	ifelse (ETC$KEYWORDVAR =="ADB01 PHONE COMPANY",392,	
	ifelse (ETC$KEYWORDVAR =="ADF99 INCIDENT",393,	
	ifelse (ETC$KEYWORDVAR =="AAG01 PRINT ACCESS CONNECT",394,	
	ifelse (ETC$KEYWORDVAR =="AFN99 ROLE",395,	
	ifelse (ETC$KEYWORDVAR =="AGK99 VIRUS",396,	
	ifelse (ETC$KEYWORDVAR =="AAK99 INSTALL",397,	
	ifelse (ETC$KEYWORDVAR =="AFX99 CALL",398,	
	ifelse (ETC$KEYWORDVAR =="AAZ99 JOB",399,	
	ifelse (ETC$KEYWORDVAR =="ACY11 APP ADMIN",400,	
	ifelse (ETC$KEYWORDVAR =="AEA99 LAN",401,	
	ifelse (ETC$KEYWORDVAR =="AAH03 LOGIN NOT WORK COMPUTER",402,	
	ifelse (ETC$KEYWORDVAR =="AAL31 SETUP COMPUTER",403,	
	ifelse (ETC$KEYWORDVAR =="ABY99 VISIO",404,	
	ifelse (ETC$KEYWORDVAR =="AAE07 EMAIL ACCOUNT",405,	
	ifelse (ETC$KEYWORDVAR =="AAB41 SAP REQUEST",406,	
	ifelse (ETC$KEYWORDVAR =="ACC02 EXCEL DOWN NOTWORK",407,	
	ifelse (ETC$KEYWORDVAR =="AEL99 RAM",408,	
	ifelse (ETC$KEYWORDVAR =="AAP05 SERVER RESTORE",409,	
	ifelse (ETC$KEYWORDVAR =="ABI01 PLM ACESS CONNECT",410,	
	ifelse (ETC$KEYWORDVAR =="AAK31 INSTALL OS",411,	
	ifelse (ETC$KEYWORDVAR =="ABG99 FIS",412,	
	ifelse (ETC$KEYWORDVAR =="AAB05 SAP SERVER",413,	
	ifelse (ETC$KEYWORDVAR =="AFD99 SALE",414,	
	ifelse (ETC$KEYWORDVAR =="AAG05 PRINT INSTALL",415,	
	ifelse (ETC$KEYWORDVAR =="ACF02 INTRANET DOWN NOTWORK",416,	
	ifelse (ETC$KEYWORDVAR =="AAB17 SAP AUTH",417,	
	ifelse (ETC$KEYWORDVAR =="AAK11 INSTALL OFFICE",418,	
	ifelse (ETC$KEYWORDVAR =="AAT03 NODE REBOOT",419,	
	ifelse (ETC$KEYWORDVAR =="ACT01 FOLDER ACESS CONNECT",420,	
	ifelse (ETC$KEYWORDVAR =="AFR99 TONER",421,	
	ifelse (ETC$KEYWORDVAR =="ABS99 PROJECT",422,	
	ifelse (ETC$KEYWORDVAR =="ACT99 FOLDER",423,	
	ifelse (ETC$KEYWORDVAR =="AGG03 PC ISSUE",424,	
	ifelse (ETC$KEYWORDVAR =="AAK15 INSTALL ADMIN",425,	
	ifelse (ETC$KEYWORDVAR =="ADB03 PHONE DOWN NOTWORK",426,	
	ifelse (ETC$KEYWORDVAR =="ABE02 TOOL ISSUE",427,	
	ifelse (ETC$KEYWORDVAR =="ACP99 SITE",428,	
	ifelse (ETC$KEYWORDVAR =="AAD06 VOICEMAIL CHANGE",429,	
	ifelse (ETC$KEYWORDVAR =="AFP99 FTP",430,	
	ifelse (ETC$KEYWORDVAR =="AAP02 SERVER DOWN NOTWORK",431,	
	ifelse (ETC$KEYWORDVAR =="AAG15 PRINT LABEL",432,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="ACW03 SYSTEM DOWN NOTWORK",433,	
	ifelse (ETC$KEYWORDVAR =="ACZ19 USER ACESS CONNECT",434,	
	ifelse (ETC$KEYWORDVAR =="ADC99 COPIER",435,	
	ifelse (ETC$KEYWORDVAR =="AAE26 EMAIL REQUEST",436,	
	ifelse (ETC$KEYWORDVAR =="ACY13 APP ISSUE",437,	
	ifelse (ETC$KEYWORDVAR =="AAH02 LOGIN NOT WORK CISCO",438,	
	ifelse (ETC$KEYWORDVAR =="AAK05 INSTALL CHINESE",439,	
	ifelse (ETC$KEYWORDVAR =="AAK10 INSTALL OUTLOOK",440,	
	ifelse (ETC$KEYWORDVAR =="ADA01 NEW HARDWARE",441,	
	ifelse (ETC$KEYWORDVAR =="AEW99 PART",442,	
	ifelse (ETC$KEYWORDVAR =="ACZ20 USER DOWN NOTWORK",443,	
	ifelse (ETC$KEYWORDVAR =="ABJ01 TS3 ACESS CONNECT",444,	
	ifelse (ETC$KEYWORDVAR =="ACU02 DRIVE DOWN NOTWORK",445,	
	ifelse (ETC$KEYWORDVAR =="AHU99 RECRUIT",446,	
	ifelse (ETC$KEYWORDVAR =="AEO99 VOLUME",447,	
	ifelse (ETC$KEYWORDVAR =="AIJ99 PAY",448,	
	ifelse (ETC$KEYWORDVAR =="AGT99 ISSUE",449,	
	ifelse (ETC$KEYWORDVAR =="AID99 TRACK",450,	
	ifelse (ETC$KEYWORDVAR =="AFJ99 CODE",451,	
	ifelse (ETC$KEYWORDVAR =="AAE04 EMAIL LOGIN",452,	
	ifelse (ETC$KEYWORDVAR =="ABK99 REFUSOL",453,	
	ifelse (ETC$KEYWORDVAR =="ABF99 SFDC",454,	
	ifelse (ETC$KEYWORDVAR =="AAK33 INSTALL SOFTWARE",455,	
	ifelse (ETC$KEYWORDVAR =="AAM04 CONFIG PHONE",456,	
	ifelse (ETC$KEYWORDVAR =="ABJ03 TS3 ISSUE",457,	
	ifelse (ETC$KEYWORDVAR =="ACK02 CAD DOWN NOTWORK",458,	
	ifelse (ETC$KEYWORDVAR =="ACX99 SERVICE",459,	
	ifelse (ETC$KEYWORDVAR =="AAA31 PASSWORD RESET SFDC",460,	
	ifelse (ETC$KEYWORDVAR =="ADJ99 SQ",461,	
	ifelse (ETC$KEYWORDVAR =="AIO99 OS",462,	
	ifelse (ETC$KEYWORDVAR =="ABL03 ASTEA ISSUE",463,	
	ifelse (ETC$KEYWORDVAR =="AEV99 MATERIAL",464,	
	ifelse (ETC$KEYWORDVAR =="AAB11 SAP ORDER",465,	
	ifelse (ETC$KEYWORDVAR =="AAP99 SERVER",466,	
	ifelse (ETC$KEYWORDVAR =="ABL01 ASTEA ACESS CONNECT",467,	
	ifelse (ETC$KEYWORDVAR =="AAK06 INSTALL ZIP",468,	
	ifelse (ETC$KEYWORDVAR =="AAF25 MAIL FULL",469,	
	ifelse (ETC$KEYWORDVAR =="AAP07 SERVER FILE",470,	
	ifelse (ETC$KEYWORDVAR =="AAB15 SAP REPORT",471,	
	ifelse (ETC$KEYWORDVAR =="AAM99 CONFIG",472,	
	ifelse (ETC$KEYWORDVAR =="ABY01 VISIO ACESS CONNECT",473,	
	ifelse (ETC$KEYWORDVAR =="AAI01 BLOCK VENDER",474,	
	ifelse (ETC$KEYWORDVAR =="ACX03 SERVICE DOWN NOTWORK",475,	
	ifelse (ETC$KEYWORDVAR =="AII99 REFU",476,	
	ifelse (ETC$KEYWORDVAR =="AEY99 ORDER",477,	
	ifelse (ETC$KEYWORDVAR =="AEF99 MOVE",478,	
	ifelse (ETC$KEYWORDVAR =="ACR99 SOLIDWORKS",479,	
	ifelse (ETC$KEYWORDVAR =="AAB09 SAP PRINT",480,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="ACV02 FILE DOWN NOTWORK",481,	
	ifelse (ETC$KEYWORDVAR =="AGF99 DESKTOP",482,	
	ifelse (ETC$KEYWORDVAR =="ACC99 EXCEL",483,	
	ifelse (ETC$KEYWORDVAR =="AGA99 BATTERY",484,	
	ifelse (ETC$KEYWORDVAR =="AHM99 LICENSE",485,	
	ifelse (ETC$KEYWORDVAR =="AIL99 MAT",486,	
	ifelse (ETC$KEYWORDVAR =="ABL99 ASTEA",487,	
	ifelse (ETC$KEYWORDVAR =="ABX99 ADP",488,	
	ifelse (ETC$KEYWORDVAR =="ACF03 INTRANET ISSUE",489,	
	ifelse (ETC$KEYWORDVAR =="ACW99 SYSTEM",490,	
	ifelse (ETC$KEYWORDVAR =="ADO99 VOIP",491,	
	ifelse (ETC$KEYWORDVAR =="AAB16 SAP CHANGE",492,	
	ifelse (ETC$KEYWORDVAR =="ADL99 ITCHECK",493,	
	ifelse (ETC$KEYWORDVAR =="ACZ15 USERS ACESS CONNECT",494,	
	ifelse (ETC$KEYWORDVAR =="AHO99 OUTPUT",495,	
	ifelse (ETC$KEYWORDVAR =="AAE10 EMAIL PHONE",496,	
	ifelse (ETC$KEYWORDVAR =="AEH99 MONITOR",497,	
	ifelse (ETC$KEYWORDVAR =="AFS99 INK",498,	
	ifelse (ETC$KEYWORDVAR =="ACE99 EXCHANGE",499,	
	ifelse (ETC$KEYWORDVAR =="AIE99 UPLOAD",500,	
	ifelse (ETC$KEYWORDVAR =="ABH99 EPDM",501,	
	ifelse (ETC$KEYWORDVAR =="ADA08 NEW ISSUE",502,	
	ifelse (ETC$KEYWORDVAR =="AES99 WG",503,	
	ifelse (ETC$KEYWORDVAR =="AFH99 FIREWALL",504,	
	ifelse (ETC$KEYWORDVAR =="AAK19 INSTALL ALTIUM",505,	
	ifelse (ETC$KEYWORDVAR =="AAB06 SAP NEW REQUEST",506,	
	ifelse (ETC$KEYWORDVAR =="ABF01 SFDC DOWN NOTWORK",507,	
	ifelse (ETC$KEYWORDVAR =="AAS05 SWITCH ISSUE",508,	
	ifelse (ETC$KEYWORDVAR =="ADI99 SNC",509,	
	ifelse (ETC$KEYWORDVAR =="AAB07 SAP NEW ACCOUNT",510,	
	ifelse (ETC$KEYWORDVAR =="ADX02 WIRELESS DOWN NOTWORK",511,	
	ifelse (ETC$KEYWORDVAR =="AEN99 SPACE",512,	
	ifelse (ETC$KEYWORDVAR =="AIK99 LOG",513,	
	ifelse (ETC$KEYWORDVAR =="AAX01 EQUIPMENT FOLLOWUP",514,	
	ifelse (ETC$KEYWORDVAR =="ABG01 FIS ACESS CONNECT",515,	
	ifelse (ETC$KEYWORDVAR =="AAK13 INSTALL SCAN",516,	
	ifelse (ETC$KEYWORDVAR =="ACU03 DRIVE ISSUE",517,	
	ifelse (ETC$KEYWORDVAR =="ABG02 FIS DOWN NOTWORK",518,	
	ifelse (ETC$KEYWORDVAR =="AAG06 PRINT TONER",519,	
	ifelse (ETC$KEYWORDVAR =="ACY03 APP DOWN NOTWORK",520,	
	ifelse (ETC$KEYWORDVAR =="ADB02 PHONE ACESS CONNECT",521,	
	ifelse (ETC$KEYWORDVAR =="ADX99 WIRELESS",522,	
	ifelse (ETC$KEYWORDVAR =="AAP09 SERVER ACCOUNT",523,	
	ifelse (ETC$KEYWORDVAR =="ABM01 BW ACESS CONNECT",524,	
	ifelse (ETC$KEYWORDVAR =="ABH03 EPDM ISSUE",525,	
	ifelse (ETC$KEYWORDVAR =="AEI99 MOUSE",526,	
	ifelse (ETC$KEYWORDVAR =="AFC99 QUOTE",527,	
	ifelse (ETC$KEYWORDVAR =="AFZ99 ADDRESS",528,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="AHA99 DATA",529,	
	ifelse (ETC$KEYWORDVAR =="ADT99 INTERFACE",530,	
	ifelse (ETC$KEYWORDVAR =="ABR01 SHAREPOINT ACESS CONNECT",531,	
	ifelse (ETC$KEYWORDVAR =="AIS99 MAINTENANCE",532,	
	ifelse (ETC$KEYWORDVAR =="AGX99 ENERGY",533,	
	ifelse (ETC$KEYWORDVAR =="AHD99 ALERT",534,	
	ifelse (ETC$KEYWORDVAR =="AHE99 CHART",535,	
	ifelse (ETC$KEYWORDVAR =="AIN99 CAR",536,	
	ifelse (ETC$KEYWORDVAR =="ACX13 SERVICE ISSUE",537,	
	ifelse (ETC$KEYWORDVAR =="ADE04 SCAN COMPUTER",538,	
	ifelse (ETC$KEYWORDVAR =="AAB10 SAP CODE",539,	
	ifelse (ETC$KEYWORDVAR =="AAF12 MAIL CHANGE",540,	
	ifelse (ETC$KEYWORDVAR =="AAG09 FIX PRINTER",541,	
	ifelse (ETC$KEYWORDVAR =="AAV05 STATION COMPUTER",542,	
	ifelse (ETC$KEYWORDVAR =="ABG03 FIS ISSUE",543,	
	ifelse (ETC$KEYWORDVAR =="AEX99 SHIP",544,	
	ifelse (ETC$KEYWORDVAR =="AAK23 INSTALL DRIVE",545,	
	ifelse (ETC$KEYWORDVAR =="ACP03 SITE ISSUE",546,	
	ifelse (ETC$KEYWORDVAR =="AAG07 PRINT INK",547,	
	ifelse (ETC$KEYWORDVAR =="AER99 DASHBOARD",548,	
	ifelse (ETC$KEYWORDVAR =="AAK17 INSTALL BW",549,	
	ifelse (ETC$KEYWORDVAR =="AAI02 BLOCK SITE",550,	
	ifelse (ETC$KEYWORDVAR =="ACW02 SYSTEM ACESS CONNECT",551,	
	ifelse (ETC$KEYWORDVAR =="AAL05 SETUP OUTLOOK",552,	
	ifelse (ETC$KEYWORDVAR =="AGH03 TIME ISSUE",553,	
	ifelse (ETC$KEYWORDVAR =="ADB04 FIX PHONE",554,	
	ifelse (ETC$KEYWORDVAR =="ADV03 PORTAL ISSUE",555,	
	ifelse (ETC$KEYWORDVAR =="ABP99 ALTIUM",556,	
	ifelse (ETC$KEYWORDVAR =="AAE13 EMAIL RESTORE",557,	
	ifelse (ETC$KEYWORDVAR =="AAH49 LOGIN ISSUE SFORCE",558,	
	ifelse (ETC$KEYWORDVAR =="AAH51 LOGIN ISSUE TOMCAT",559,	
	ifelse (ETC$KEYWORDVAR =="ABN99 BOM",560,	
	ifelse (ETC$KEYWORDVAR =="AAG14 PRINT JOB",561,	
	ifelse (ETC$KEYWORDVAR =="AAH50 LOGIN ISSUE SFDC",562,	
	ifelse (ETC$KEYWORDVAR =="ACM02 SQL DOWN NOTWORK",563,	
	ifelse (ETC$KEYWORDVAR =="AAE19 EMAIL JUNK",564,	
	ifelse (ETC$KEYWORDVAR =="AAP04 SERVER BACKUP",565,	
	ifelse (ETC$KEYWORDVAR =="AAJ06 LOCK FIS",566,	
	ifelse (ETC$KEYWORDVAR =="AAL35 SETUP SOFTWARE",567,	
	ifelse (ETC$KEYWORDVAR =="AAA42 PASSWORD FIS",568,	
	ifelse (ETC$KEYWORDVAR =="ABK02 REFUSOL DOWN NOTWORK",569,	
	ifelse (ETC$KEYWORDVAR =="ADA06 NEW SNC",570,	
	ifelse (ETC$KEYWORDVAR =="AAV04 STATION ADMIN",571,	
	ifelse (ETC$KEYWORDVAR =="ADU99 QUEUE",572,	
	ifelse (ETC$KEYWORDVAR =="AAA34 PASSWORD ITCHECK",573,	
	ifelse (ETC$KEYWORDVAR =="AAU01 LINK ACESS CONNECT",574,	
	ifelse (ETC$KEYWORDVAR =="ADD09 FAX ISSUE",575,	
	ifelse (ETC$KEYWORDVAR =="AAB23 SAP EXCEL",576,	paste(ETC$KEYWORDVAR_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O2 <-	ifelse (ETC$KEYWORDVAR =="ADH01 DEBUG REQ",577,	
	ifelse (ETC$KEYWORDVAR =="AAS01 SWITCH ACESS CONNECT",578,	
	ifelse (ETC$KEYWORDVAR =="ADD02 FAX DOWN NOTWORK",579,	
	ifelse (ETC$KEYWORDVAR =="ADB06 PHONE ADMIN",580,	
	ifelse (ETC$KEYWORDVAR =="AAL18 SETUP SKYPE",581,	
	ifelse (ETC$KEYWORDVAR =="AFI99 BARCODE",582,	
	ifelse (ETC$KEYWORDVAR =="AAH40 LOGIN ISSUE FIS",583,	
	ifelse (ETC$KEYWORDVAR =="AAR99 ROUTER",584,	
	ifelse (ETC$KEYWORDVAR =="AAP08 SERVER ROLLOUT",585,	
	ifelse (ETC$KEYWORDVAR =="ADE01 SCAN ACESS CONNECT",586,	
	ifelse (ETC$KEYWORDVAR =="ACY09 APP ROLLOUT",587,	
	ifelse (ETC$KEYWORDVAR =="AAL09 SETUP ADMIN",588,	paste(ETC$KEYWORDVAR_O2)))))))))))))
	
ETC$KEYWORDVAR_O2 <- ifelse (ETC$KEYWORDVAR_O2 == "NA", 383, paste(ETC$KEYWORDVAR_O2))

ETC$KEYWORDVAR_R2 <- "NA"

		
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AAK21 INSTALL CITRIX",1,	
	ifelse (ETC$KEYWORDVAR=="ADE02 SCAN DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADY03 WIFI ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAV01 STATION ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ACK99 CAD",1,	
	ifelse (ETC$KEYWORDVAR=="AAA28 PASSWORD VPN",1,	
	ifelse (ETC$KEYWORDVAR=="AAF09 MAIL SCAN",1,	
	ifelse (ETC$KEYWORDVAR=="AAP03 SERVER PATCH",1,	
	ifelse (ETC$KEYWORDVAR=="AAA25 PASSWORD RESET ADP",1,	
	ifelse (ETC$KEYWORDVAR=="AAA44 PASSWORD BW",1,	
	ifelse (ETC$KEYWORDVAR=="AAD02 VOICEMAIL ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAG08 NEW PRINTER",1,	
	ifelse (ETC$KEYWORDVAR=="AAH26 LOGIN LONG",1,	
	ifelse (ETC$KEYWORDVAR=="AAL06 SETUP PROJECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAM03 CONFIG ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="ADV01 PORTAL ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AFF99 BILL",1,	
	ifelse (ETC$KEYWORDVAR=="AAF04 MAIL LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAA46 PASSWORD AE",1,	
	ifelse (ETC$KEYWORDVAR=="ABR02 SHAREPOINT DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA13 PASSWORD RESET WINDOWS",1,	
	ifelse (ETC$KEYWORDVAR=="AAH43 LOGIN ISSUE LAPTOP",1,	
	ifelse (ETC$KEYWORDVAR=="ABN03 BOM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ADE10 SCAN ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAH08 LOGIN NOT WORK EXCHANGE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA39 PASSWORD RESET TS3",1,	
	ifelse (ETC$KEYWORDVAR=="AAH25 LOGIN ACCESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAH31 LOGIN ISSUE BW",1,	
	ifelse (ETC$KEYWORDVAR=="ADK99 ACROBAT",1,	
	ifelse (ETC$KEYWORDVAR=="AAL15 SETUP DELL",1,	
	ifelse (ETC$KEYWORDVAR=="AHS99 PRODUCT",1,	
	ifelse (ETC$KEYWORDVAR=="ADD04 FAX COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="AAA01 PASSWORD RESET EMAIL",1,	
	ifelse (ETC$KEYWORDVAR=="AAA20 PASSWORD ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="AAA35 PASSWORD RESET PCARD",1,	
	ifelse (ETC$KEYWORDVAR=="AAD04 VOICEMAIL LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAH36 LOGIN ISSUE EMPLOYEE",1,	
	ifelse (ETC$KEYWORDVAR=="AAU05 LINK ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ABS02 PROJECT DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA05 PASSWORD RESET NETWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA40 PASSWORD TS3",1,	
	ifelse (ETC$KEYWORDVAR=="AAG16 PRINT INVOICE",1,	
	ifelse (ETC$KEYWORDVAR=="AAH05 LOGIN NOT WORK DOMAIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAA47 PASSWORD RESET AUTO REPLY",1,	
	ifelse (ETC$KEYWORDVAR=="AAL22 SETUP STATION",1,	
	ifelse (ETC$KEYWORDVAR=="ADX03 WIRELESS ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA45 PASSWORD RESET AE",1,	
	ifelse (ETC$KEYWORDVAR=="AAL20 SETUP WIRELESS",1,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AAP10 SERVER ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="AGY99 DONGLE",1,	
	ifelse (ETC$KEYWORDVAR=="AIB99 TERMINATION",1,	
	ifelse (ETC$KEYWORDVAR=="AAA33 PASSWORD RESET ITCHECK",1,	
	ifelse (ETC$KEYWORDVAR=="AAF26 MAIL ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ABZ02 SALESFORCE DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA29 PASSWORD RESET ASTEA",1,	
	ifelse (ETC$KEYWORDVAR=="AAA11 PASSWORD RESET COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="ABO99 FUSIONOPS",1,	
	ifelse (ETC$KEYWORDVAR=="AFB99 PURCHASE",1,	
	ifelse (ETC$KEYWORDVAR=="AIR99 TEST",1,	
	ifelse (ETC$KEYWORDVAR=="AAF10 MAIL PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAH04 LOGIN NOT WORK DELL",1,	
	ifelse (ETC$KEYWORDVAR=="AAA03 PASSWORD RESET EXCHANGE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA24 PASSWORD USER",1,	
	ifelse (ETC$KEYWORDVAR=="ABI03 PLM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ACA99 MS OFFICE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA19 PASSWORD RESET ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="ABM02 BW DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ABM03 BW ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ACB02 OFFICE DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAB37 SAP BW",1,	
	ifelse (ETC$KEYWORDVAR=="AAF07 MAIL ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="AAH53 LOGIN ISSUE VPN",1,	
	ifelse (ETC$KEYWORDVAR=="ABL02 ASTEA DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AEP99 PAYROLL",1,	
	ifelse (ETC$KEYWORDVAR=="ABZ03 SALESFORCE ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ACD02 OUTLOOK DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACW07 SYSTEM VIRUS HACK",1,	
	ifelse (ETC$KEYWORDVAR=="ADM99 FLASH",1,	
	ifelse (ETC$KEYWORDVAR=="AGD01 COMPUTER ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AHQ99 PROCESS",1,	
	ifelse (ETC$KEYWORDVAR=="AIQ99 TRAINING",1,	
	ifelse (ETC$KEYWORDVAR=="AAA02 PASSWORD EMAIL",1,	
	ifelse (ETC$KEYWORDVAR=="AAJ07 LOCK PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK16 INSTALL CAD",1,	
	ifelse (ETC$KEYWORDVAR=="ACF01 INTRANET ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ACW11 SYSTEM ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="ADY02 WIFI DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA27 PASSWORD RESET VPN",1,	
	ifelse (ETC$KEYWORDVAR=="AAC04 WEBEX LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAF01 MAIL DELIVERY",1,	
	ifelse (ETC$KEYWORDVAR=="AAK04 INSTALL VPN",1,	
	ifelse (ETC$KEYWORDVAR=="AAM07 CONFIG COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="ABJ02 TS3 DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACQ99 MCAFEE",1,	
	ifelse (ETC$KEYWORDVAR=="ADQ99 REPORT",1,	
	ifelse (ETC$KEYWORDVAR=="AHW99 RESTART",1,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AAA16 PASSWORD PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAB35 SAP TS3",1,	
	ifelse (ETC$KEYWORDVAR=="AGG02 PC DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAA09 PASSWORD RESET WEBEX",1,	
	ifelse (ETC$KEYWORDVAR=="AAC09 WEBEX AUTH",1,	
	ifelse (ETC$KEYWORDVAR=="AAQ14 NETWORK ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AGW99 QUICKEN",1,	
	ifelse (ETC$KEYWORDVAR=="AAC16 WEBEX REQUEST",1,	
	ifelse (ETC$KEYWORDVAR=="ADC01 COPIER ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AHY99 SCREEN",1,	
	ifelse (ETC$KEYWORDVAR=="AAB18 SAP RESET",1,	
	ifelse (ETC$KEYWORDVAR=="AAB32 SAP PORTAL",1,	
	ifelse (ETC$KEYWORDVAR=="AAB33 SAP REFUSOL",1,	
	ifelse (ETC$KEYWORDVAR=="ABE01 TOOL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAE09 EMAIL SCAN",1,	
	ifelse (ETC$KEYWORDVAR=="ADE99 SCAN",1,	
	ifelse (ETC$KEYWORDVAR=="AAW99 VM",1,	
	ifelse (ETC$KEYWORDVAR=="ADX01 WIRELESS ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAL13 SETUP CISCO",1,	
	ifelse (ETC$KEYWORDVAR=="ABU99 ADOBE",1,	
	ifelse (ETC$KEYWORDVAR=="ADB08 PHONE CHANGE",1,	
	ifelse (ETC$KEYWORDVAR=="ADY99 WIFI",1,	
	ifelse (ETC$KEYWORDVAR=="AFQ99 EFT",1,	
	ifelse (ETC$KEYWORDVAR=="AIG99 WEB",1,	
	ifelse (ETC$KEYWORDVAR=="AAC02 WEBEX ACESS",1,	
	ifelse (ETC$KEYWORDVAR=="AAE14 EMAIL NOTIFICATION",1,	
	ifelse (ETC$KEYWORDVAR=="AAK08 INSTALL VISIO",1,	
	ifelse (ETC$KEYWORDVAR=="ABV99 GOOGLE",1,	
	ifelse (ETC$KEYWORDVAR=="ACC01 EXCEL ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ADG01 CRITICAL ALERT",1,	
	ifelse (ETC$KEYWORDVAR=="ADZ99 REMOTE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA49 PASSWORD RESET",2,	
	ifelse (ETC$KEYWORDVAR=="AAH27 NEW LOGIN",2,	
	ifelse (ETC$KEYWORDVAR=="AAH15 LOGIN NOT WORK OUTLOOK",2,	
	ifelse (ETC$KEYWORDVAR=="AAK12 INSTALL PROJECT",2,	
	ifelse (ETC$KEYWORDVAR=="ACG99 EXTRANET",2,	
	ifelse (ETC$KEYWORDVAR=="AHV99 RESET",2,	
	ifelse (ETC$KEYWORDVAR=="AAC08 WEBEX CHANGE",2,	
	ifelse (ETC$KEYWORDVAR=="AAJ10 LOCK ACCOUNT",2,	
	ifelse (ETC$KEYWORDVAR=="AAA08 PASSWORD SAP",2,	
	ifelse (ETC$KEYWORDVAR=="AAH20 LOGIN NOT WORK SFDC",2,	
	ifelse (ETC$KEYWORDVAR=="AAQ99 NETWORK",2,	
	ifelse (ETC$KEYWORDVAR=="ADZ01 REMOTE ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="ACP01 SITE ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AGF02 DESKTOP DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR=="ACH01 DELL ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AAO99 VPN",2,	
	ifelse (ETC$KEYWORDVAR=="AAP11 SERVER MAINTENANCE",2,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="ADB09 PHONE DELIVERY",2,	
	ifelse (ETC$KEYWORDVAR=="AAB02 SAP DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR=="AAX03 EQUIPMENT ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACZ17 USERS ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="AAN03 INTERNET ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="AAA07 PASSWORD RESET SAP",2,	
	ifelse (ETC$KEYWORDVAR=="AAA99 PASSWORD",2,	
	ifelse (ETC$KEYWORDVAR=="AAL12 SETUP PHONE",2,	
	ifelse (ETC$KEYWORDVAR=="ADP99 USB",2,	
	ifelse (ETC$KEYWORDVAR=="AHN99 MESSAGE",2,	
	ifelse (ETC$KEYWORDVAR=="AAA36 PASSWORD PCARD",2,	
	ifelse (ETC$KEYWORDVAR=="AAE25 EMAIL ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="AAK27 INSTALL SKYPE",2,	
	ifelse (ETC$KEYWORDVAR=="AAK30 INSTALL IE",2,	
	ifelse (ETC$KEYWORDVAR=="AAQ10 NETWORK COMPUTER",2,	
	ifelse (ETC$KEYWORDVAR=="ACF99 INTRANET",2,	
	ifelse (ETC$KEYWORDVAR=="ACI01 CISCO ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AFK99 GROUP",2,	
	ifelse (ETC$KEYWORDVAR=="AHI99 GEAR",2,	
	ifelse (ETC$KEYWORDVAR=="ACB03 OFFICE ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACC03 EXCEL ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ADW99 DOMAIN",2,	
	ifelse (ETC$KEYWORDVAR=="AAC01 WEBEX DISABLE",2,	
	ifelse (ETC$KEYWORDVAR=="AAC06 WEBEX ACCOUNT",2,	
	ifelse (ETC$KEYWORDVAR=="AEJ99 HEADSET",2,	
	ifelse (ETC$KEYWORDVAR=="AAG02 PRINT DOWN NOTWORK PROD",3,	
	ifelse (ETC$KEYWORDVAR=="AAO02 VPN DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="AAF02 MAIL ACCESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AAK03 INSTALL FIS",3,	
	ifelse (ETC$KEYWORDVAR=="AAL02 SETUP VPN",3,	
	ifelse (ETC$KEYWORDVAR=="AAA10 PASSWORD WEBEX",3,	
	ifelse (ETC$KEYWORDVAR=="AAC14 WEBEX TOOL",3,	
	ifelse (ETC$KEYWORDVAR=="AAJ03 LOCK CISCO",3,	
	ifelse (ETC$KEYWORDVAR=="ADG99 CRITICAL",3,	
	ifelse (ETC$KEYWORDVAR=="AHF99 DIRECTORY",3,	
	ifelse (ETC$KEYWORDVAR=="AAK25 INSTALL EXCEL",3,	
	ifelse (ETC$KEYWORDVAR=="ADB11 PHONE DIRECTORY",3,	
	ifelse (ETC$KEYWORDVAR=="AGM99 SPAM",3,	
	ifelse (ETC$KEYWORDVAR=="AHG99 FIRM",3,	
	ifelse (ETC$KEYWORDVAR=="ADD99 FAX",3,	
	ifelse (ETC$KEYWORDVAR=="AEA01 LAN ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AGG01 PC ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AAA43 PASSWORD RESET BW",3,	
	ifelse (ETC$KEYWORDVAR=="AAB38 SAP TOOL",3,	
	ifelse (ETC$KEYWORDVAR=="AAF03 MAIL DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ABI99 PLM",3,	
	ifelse (ETC$KEYWORDVAR=="AAC12 WEBEX MEETING",3,	
	ifelse (ETC$KEYWORDVAR=="ACP02 SITE DOWN NOTWORK",3,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AGB99 POWER",3,	
	ifelse (ETC$KEYWORDVAR=="AAK29 INSTALL EXPENS",3,	
	ifelse (ETC$KEYWORDVAR=="ABA02 WINZIP DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ACB01 OFFICE ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ADR99 SOFTWARE",3,	
	ifelse (ETC$KEYWORDVAR=="AAH23 LOGIN NOT WORK VPN",3,	
	ifelse (ETC$KEYWORDVAR=="AAK26 INSTALL PDF",3,	
	ifelse (ETC$KEYWORDVAR=="ACX10 SERVICE ACCOUNT",3,	
	ifelse (ETC$KEYWORDVAR=="AGH02 TIME DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="AGZ99 DATABASE",3,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP INSTALL",3,	
	ifelse (ETC$KEYWORDVAR=="AAC15 WEBEX ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAW01 VM ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AHR99 PRODUCTION",3,	
	ifelse (ETC$KEYWORDVAR=="AAK14 INSTALL FAX",3,	
	ifelse (ETC$KEYWORDVAR=="ACZ13 NEW HIRE COMPUTER",3,	
	ifelse (ETC$KEYWORDVAR=="AEB99 BOX",3,	
	ifelse (ETC$KEYWORDVAR=="ABS01 PROJECT ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AGD03 COMPUTER ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAC05 WEBEX INSTALL",3,	
	ifelse (ETC$KEYWORDVAR=="AAK02 INSTALL EPDM",3,	
	ifelse (ETC$KEYWORDVAR=="AAK22 INSTALL DELL",3,	
	ifelse (ETC$KEYWORDVAR=="ACV03 FILE ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AFG99 DELIVER",3,	
	ifelse (ETC$KEYWORDVAR=="AAB03 SAP LOGIN",3,	
	ifelse (ETC$KEYWORDVAR=="AAB13 SAP ITEM",3,	
	ifelse (ETC$KEYWORDVAR=="AAH30 LOGIN ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAH34 LOGIN ISSUE DELL",3,	
	ifelse (ETC$KEYWORDVAR=="ACO99 WEBSITE",3,	
	ifelse (ETC$KEYWORDVAR=="AAD01 VOICEMAIL DISABLE",3,	
	ifelse (ETC$KEYWORDVAR=="AAJ99 LOCK",3,	
	ifelse (ETC$KEYWORDVAR=="ABA99 WINZIP",3,	
	ifelse (ETC$KEYWORDVAR=="ADN99 CPU",3,	
	ifelse (ETC$KEYWORDVAR=="AAC99 WEBEX",3,	
	ifelse (ETC$KEYWORDVAR=="AAT99 NODE",3,	
	ifelse (ETC$KEYWORDVAR=="ACM01 SQL ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ACO02 WEBSITE DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ABH01 EPDM ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ACD03 OUTLOOK ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="ACH99 DELL",3,	
	ifelse (ETC$KEYWORDVAR=="AAE12 EMAIL CHANGE",3,	
	ifelse (ETC$KEYWORDVAR=="ABT99 PDF",3,	
	ifelse (ETC$KEYWORDVAR=="ACI99 CISCO",3,	
	ifelse (ETC$KEYWORDVAR=="AGU99 HELP DESK",3,	
	ifelse (ETC$KEYWORDVAR=="AAA48 PASSWORD RESET AUTO",4,	
	ifelse (ETC$KEYWORDVAR=="AAD99 VOICEMAIL",4,	
	ifelse (ETC$KEYWORDVAR=="ABR99 SHAREPOINT",4,	
	ifelse (ETC$KEYWORDVAR=="AHX99 RESOURCE",4,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AAV02 STATION DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="ACZ14 NEW HIRE",4,	
	ifelse (ETC$KEYWORDVAR=="AAF05 MAIL INSTALL",4,	
	ifelse (ETC$KEYWORDVAR=="ACO01 WEBSITE ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="ACV01 FILE ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="ACX02 SERVICE ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AFL99 CASE",4,	
	ifelse (ETC$KEYWORDVAR=="AFW99 CONFERENCE",4,	
	ifelse (ETC$KEYWORDVAR=="ADY01 WIFI ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AHT99 RECORD",4,	
	ifelse (ETC$KEYWORDVAR=="AAA12 PASSWORD COMPUTER",4,	
	ifelse (ETC$KEYWORDVAR=="ACU99 DRIVE",4,	
	ifelse (ETC$KEYWORDVAR=="AGO99 DOWN",4,	
	ifelse (ETC$KEYWORDVAR=="ADB05 PHONE COMPUTER",4,	
	ifelse (ETC$KEYWORDVAR=="AIH99 TICKET",4,	
	ifelse (ETC$KEYWORDVAR=="AAL19 SETUP EXPENS",4,	
	ifelse (ETC$KEYWORDVAR=="AAY99 INVENTORY",4,	
	ifelse (ETC$KEYWORDVAR=="ADW01 DOMAIN ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AAC17 WEBEX CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AAL27 SETUP TS3",4,	
	ifelse (ETC$KEYWORDVAR=="AAP12 SERVER ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="ADA02 NEW GENERAL",4,	
	ifelse (ETC$KEYWORDVAR=="ADB12 PHONE ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="AFE99 TAX",4,	
	ifelse (ETC$KEYWORDVAR=="AAH28 LOGIN CHANGE",4,	
	ifelse (ETC$KEYWORDVAR=="AAQ01 NETWORK WIRELESS",4,	
	ifelse (ETC$KEYWORDVAR=="ABE99 TOOL",4,	
	ifelse (ETC$KEYWORDVAR=="AGD02 COMPUTER DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="AAC11 WEBEX OUTLOOK",4,	
	ifelse (ETC$KEYWORDVAR=="AAN99 INTERNET",4,	
	ifelse (ETC$KEYWORDVAR=="AAV99 STATION",4,	
	ifelse (ETC$KEYWORDVAR=="AAF20 MAIL OUTLOOK",4,	
	ifelse (ETC$KEYWORDVAR=="AAK09 INSTALL GOOGLE",4,	
	ifelse (ETC$KEYWORDVAR=="ABF02 SFDC ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="ACY10 APP ACCOUNT",4,	
	ifelse (ETC$KEYWORDVAR=="AAK07 INSTALL WINDOWS",4,	
	ifelse (ETC$KEYWORDVAR=="AFV99 MEET",4,	
	ifelse (ETC$KEYWORDVAR=="AAA23 PASSWORD RESET USER",4,	
	ifelse (ETC$KEYWORDVAR=="AAQ03 NETWORK DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="ABD02 PTOOL ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="AAG99 PRINT",4,	
	ifelse (ETC$KEYWORDVAR=="ABQ99 ENGINEER",4,	
	ifelse (ETC$KEYWORDVAR=="ACZ06 NEW HIRE PHONE",4,	
	ifelse (ETC$KEYWORDVAR=="AAH24 LOGIN NOT WORK",4,	
	ifelse (ETC$KEYWORDVAR=="ABR03 SHAREPOINT ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="AAO01 VPN ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AAT02 NODE DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="AFA99 INVOICE",4,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AAJ04 LOCK COMPUTER",4,	
	ifelse (ETC$KEYWORDVAR=="AAK18 INSTALL ADOBE",4,	
	ifelse (ETC$KEYWORDVAR=="AAE99 EMAIL",4,	
	ifelse (ETC$KEYWORDVAR=="AAS99 SWITCH",4,	
	ifelse (ETC$KEYWORDVAR=="AFM99 LIST",4,	
	ifelse (ETC$KEYWORDVAR=="AAC10 WEBEX SETTING",4,	
	ifelse (ETC$KEYWORDVAR=="AAG12 PRINT CHANGE",4,	
	ifelse (ETC$KEYWORDVAR=="AEU99 PORT",4,	
	ifelse (ETC$KEYWORDVAR=="AGP99 ACCESS",4,	
	ifelse (ETC$KEYWORDVAR=="AAL99 SETUP",4,	
	ifelse (ETC$KEYWORDVAR=="AAU02 LINK DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="AAM01 CONFIG OUTLOOK",4,	
	ifelse (ETC$KEYWORDVAR=="AAZ01 JOB STEP1",5,	
	ifelse (ETC$KEYWORDVAR=="AGG99 PC",5,	
	ifelse (ETC$KEYWORDVAR=="AAE08 EMAIL PRINT",5,	
	ifelse (ETC$KEYWORDVAR=="AAO03 VPN ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="ACM99 SQL",5,	
	ifelse (ETC$KEYWORDVAR=="AEM99 MEMORY",5,	
	ifelse (ETC$KEYWORDVAR=="AAF99 MAIL",5,	
	ifelse (ETC$KEYWORDVAR=="AAQ02 NETWORK ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="ABQ01 ENGINEER ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="ADH99 REQ",5,	
	ifelse (ETC$KEYWORDVAR=="AAK01 INSTALL PLM",5,	
	ifelse (ETC$KEYWORDVAR=="AAH07 LOGIN NOT WORK EPMD",5,	
	ifelse (ETC$KEYWORDVAR=="AAI99 BLOCK",5,	
	ifelse (ETC$KEYWORDVAR=="AAL26 SETUP TECHNICIAN",5,	
	ifelse (ETC$KEYWORDVAR=="AAN02 INTERNET DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="AAE16 EMAIL GROUP",5,	
	ifelse (ETC$KEYWORDVAR=="ADF01 INCIDENT ATTACK",5,	
	ifelse (ETC$KEYWORDVAR=="AAN01 INTERNET ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AAH17 LOGIN NOT WORK PORTAL",5,	
	ifelse (ETC$KEYWORDVAR=="AAH52 LOGIN ISSUE TOOL",5,	
	ifelse (ETC$KEYWORDVAR=="AAQ08 NETWORK FILE",5,	
	ifelse (ETC$KEYWORDVAR=="ABC99 WIN",5,	
	ifelse (ETC$KEYWORDVAR=="AIP99 IE",5,	
	ifelse (ETC$KEYWORDVAR=="ACW13 SYSTEM ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="AFO99 BACKUP",5,	
	ifelse (ETC$KEYWORDVAR=="AAE02 EMAIL ACCESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AAG20 PRINT ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="AAK24 INSTALL ENGINEER",5,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW PHONE",5,	
	ifelse (ETC$KEYWORDVAR=="ADA07 NEW COMPUTER",5,	
	ifelse (ETC$KEYWORDVAR=="ACK01 CAD ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AEQ99 PTO",5,	
	ifelse (ETC$KEYWORDVAR=="AGS99 CHANGE",5,	
	ifelse (ETC$KEYWORDVAR=="AAG03 PRINT DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="AAL21 SETUP TABLET",5,	
	ifelse (ETC$KEYWORDVAR=="ADV02 PORTAL DOWN NOTWORK",5,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AEG99 KEYBOARD",5,	
	ifelse (ETC$KEYWORDVAR=="AGD99 COMPUTER",5,	
	ifelse (ETC$KEYWORDVAR=="AAE05 EMAIL INSTALL",5,	
	ifelse (ETC$KEYWORDVAR=="AAH99 LOGIN",5,	
	ifelse (ETC$KEYWORDVAR=="AAU99 LINK",5,	
	ifelse (ETC$KEYWORDVAR=="ABM99 BW",5,	
	ifelse (ETC$KEYWORDVAR=="ACZ18 USERS",5,	
	ifelse (ETC$KEYWORDVAR=="AGL99 MALWARE",5,	
	ifelse (ETC$KEYWORDVAR=="AGQ99 ADMIN",5,	
	ifelse (ETC$KEYWORDVAR=="AAE03 EMAIL DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="ADA99 NEW",5,	
	ifelse (ETC$KEYWORDVAR=="AFU99 INFO",5,	
	ifelse (ETC$KEYWORDVAR=="AAB01 SAP ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="ACA02 MS OFFICE DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="ABD99 PTOOL",5,	
	ifelse (ETC$KEYWORDVAR=="AAA15 PASSWORD RESET PHONE",6,	
	ifelse (ETC$KEYWORDVAR=="AAL07 SETUP SCAN",6,	
	ifelse (ETC$KEYWORDVAR=="ABJ99 TS3",6,	
	ifelse (ETC$KEYWORDVAR=="ACD99 OUTLOOK",6,	
	ifelse (ETC$KEYWORDVAR=="ACV99 FILE",6,	
	ifelse (ETC$KEYWORDVAR=="ADB99 PHONE",6,	
	ifelse (ETC$KEYWORDVAR=="AAE20 EMAIL OUTLOOK",6,	
	ifelse (ETC$KEYWORDVAR=="AAH33 LOGIN ISSUE COMPUTER",6,	
	ifelse (ETC$KEYWORDVAR=="ABK01 REFUSOL ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="ADA04 NEW SOFTWARE",6,	
	ifelse (ETC$KEYWORDVAR=="AET99 EXPENSE",6,	
	ifelse (ETC$KEYWORDVAR=="ACU01 DRIVE ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AFT99 DOC",6,	
	ifelse (ETC$KEYWORDVAR=="ACD01 OUTLOOK ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AAC03 WEBEX DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="ABZ99 SALESFORCE",6,	
	ifelse (ETC$KEYWORDVAR=="ACY02 APP ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AFY99 CALENDAR",6,	
	ifelse (ETC$KEYWORDVAR=="AAF06 MAIL SERVER",6,	
	ifelse (ETC$KEYWORDVAR=="AAH13 LOGIN NOT WORK LAPTOP",6,	
	ifelse (ETC$KEYWORDVAR=="ACZ12 NEW HIRE ACCOUNT",6,	
	ifelse (ETC$KEYWORDVAR=="ACB99 OFFICE",6,	
	ifelse (ETC$KEYWORDVAR=="ACZ99 USER",6,	
	ifelse (ETC$KEYWORDVAR=="AAQ05 NETWORK BACKUP",6,	
	ifelse (ETC$KEYWORDVAR=="AGH99 TIME",6,	
	ifelse (ETC$KEYWORDVAR=="AAX99 EQUIPMENT",6,	
	ifelse (ETC$KEYWORDVAR=="ABB99 WINDOWS",6,	
	ifelse (ETC$KEYWORDVAR=="AAB40 SAP ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="AAA37 PASSWORD RESET PC",6,	
	ifelse (ETC$KEYWORDVAR=="ACH03 DELL ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="ACY99 APP",6,	
	ifelse (ETC$KEYWORDVAR=="OTHER",6,	
	ifelse (ETC$KEYWORDVAR=="AAE01 EMAIL DELIVERY",7,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="ACZ21 USER ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AAB08 SAP ACCOUNT",7,	
	ifelse (ETC$KEYWORDVAR=="AAB99 SAP",7,	
	ifelse (ETC$KEYWORDVAR=="AAV06 STATION ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ADV99 PORTAL",7,	
	ifelse (ETC$KEYWORDVAR=="ABD01 PTOOL DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AAP01 SERVER ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="ADB01 PHONE COMPANY",7,	
	ifelse (ETC$KEYWORDVAR=="ADF99 INCIDENT",7,	
	ifelse (ETC$KEYWORDVAR=="AAG01 PRINT ACCESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AFN99 ROLE",7,	
	ifelse (ETC$KEYWORDVAR=="AGK99 VIRUS",7,	
	ifelse (ETC$KEYWORDVAR=="AAK99 INSTALL",7,	
	ifelse (ETC$KEYWORDVAR=="AFX99 CALL",7,	
	ifelse (ETC$KEYWORDVAR=="AAZ99 JOB",7,	
	ifelse (ETC$KEYWORDVAR=="ACY11 APP ADMIN",7,	
	ifelse (ETC$KEYWORDVAR=="AEA99 LAN",7,	
	ifelse (ETC$KEYWORDVAR=="AAH03 LOGIN NOT WORK COMPUTER",7,	
	ifelse (ETC$KEYWORDVAR=="AAL31 SETUP COMPUTER",7,	
	ifelse (ETC$KEYWORDVAR=="ABY99 VISIO",7,	
	ifelse (ETC$KEYWORDVAR=="AAE07 EMAIL ACCOUNT",7,	
	ifelse (ETC$KEYWORDVAR=="AAB41 SAP REQUEST",7,	
	ifelse (ETC$KEYWORDVAR=="ACC02 EXCEL DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AEL99 RAM",7,	
	ifelse (ETC$KEYWORDVAR=="AAP05 SERVER RESTORE",7,	
	ifelse (ETC$KEYWORDVAR=="ABI01 PLM ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AAK31 INSTALL OS",7,	
	ifelse (ETC$KEYWORDVAR=="ABG99 FIS",7,	
	ifelse (ETC$KEYWORDVAR=="AAB05 SAP SERVER",7,	
	ifelse (ETC$KEYWORDVAR=="AFD99 SALE",7,	
	ifelse (ETC$KEYWORDVAR=="AAG05 PRINT INSTALL",7,	
	ifelse (ETC$KEYWORDVAR=="ACF02 INTRANET DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AAB17 SAP AUTH",7,	
	ifelse (ETC$KEYWORDVAR=="AAK11 INSTALL OFFICE",7,	
	ifelse (ETC$KEYWORDVAR=="AAT03 NODE REBOOT",7,	
	ifelse (ETC$KEYWORDVAR=="ACT01 FOLDER ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AFR99 TONER",7,	
	ifelse (ETC$KEYWORDVAR=="ABS99 PROJECT",7,	
	ifelse (ETC$KEYWORDVAR=="ACT99 FOLDER",7,	
	ifelse (ETC$KEYWORDVAR=="AGG03 PC ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AAK15 INSTALL ADMIN",7,	
	ifelse (ETC$KEYWORDVAR=="ADB03 PHONE DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="ABE02 TOOL ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ACP99 SITE",7,	
	ifelse (ETC$KEYWORDVAR=="AAD06 VOICEMAIL CHANGE",7,	
	ifelse (ETC$KEYWORDVAR=="AFP99 FTP",7,	
	ifelse (ETC$KEYWORDVAR=="AAP02 SERVER DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AAG15 PRINT LABEL",7,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="ACW03 SYSTEM DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="ACZ19 USER ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="ADC99 COPIER",7,	
	ifelse (ETC$KEYWORDVAR=="AAE26 EMAIL REQUEST",7,	
	ifelse (ETC$KEYWORDVAR=="ACY13 APP ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AAH02 LOGIN NOT WORK CISCO",7,	
	ifelse (ETC$KEYWORDVAR=="AAK05 INSTALL CHINESE",7,	
	ifelse (ETC$KEYWORDVAR=="AAK10 INSTALL OUTLOOK",7,	
	ifelse (ETC$KEYWORDVAR=="ADA01 NEW HARDWARE",7,	
	ifelse (ETC$KEYWORDVAR=="AEW99 PART",7,	
	ifelse (ETC$KEYWORDVAR=="ACZ20 USER DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="ABJ01 TS3 ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="ACU02 DRIVE DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AHU99 RECRUIT",7,	
	ifelse (ETC$KEYWORDVAR=="AEO99 VOLUME",7,	
	ifelse (ETC$KEYWORDVAR=="AIJ99 PAY",7,	
	ifelse (ETC$KEYWORDVAR=="AGT99 ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AID99 TRACK",8,	
	ifelse (ETC$KEYWORDVAR=="AFJ99 CODE",8,	
	ifelse (ETC$KEYWORDVAR=="AAE04 EMAIL LOGIN",8,	
	ifelse (ETC$KEYWORDVAR=="ABK99 REFUSOL",8,	
	ifelse (ETC$KEYWORDVAR=="ABF99 SFDC",8,	
	ifelse (ETC$KEYWORDVAR=="AAK33 INSTALL SOFTWARE",8,	
	ifelse (ETC$KEYWORDVAR=="AAM04 CONFIG PHONE",8,	
	ifelse (ETC$KEYWORDVAR=="ABJ03 TS3 ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ACK02 CAD DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ACX99 SERVICE",8,	
	ifelse (ETC$KEYWORDVAR=="AAA31 PASSWORD RESET SFDC",8,	
	ifelse (ETC$KEYWORDVAR=="ADJ99 SQ",8,	
	ifelse (ETC$KEYWORDVAR=="AIO99 OS",8,	
	ifelse (ETC$KEYWORDVAR=="ABL03 ASTEA ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AEV99 MATERIAL",8,	
	ifelse (ETC$KEYWORDVAR=="AAB11 SAP ORDER",8,	
	ifelse (ETC$KEYWORDVAR=="AAP99 SERVER",8,	
	ifelse (ETC$KEYWORDVAR=="ABL01 ASTEA ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAK06 INSTALL ZIP",8,	
	ifelse (ETC$KEYWORDVAR=="AAF25 MAIL FULL",8,	
	ifelse (ETC$KEYWORDVAR=="AAP07 SERVER FILE",8,	
	ifelse (ETC$KEYWORDVAR=="AAB15 SAP REPORT",8,	
	ifelse (ETC$KEYWORDVAR=="AAM99 CONFIG",8,	
	ifelse (ETC$KEYWORDVAR=="ABY01 VISIO ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAI01 BLOCK VENDER",8,	
	ifelse (ETC$KEYWORDVAR=="ACX03 SERVICE DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AII99 REFU",8,	
	ifelse (ETC$KEYWORDVAR=="AEY99 ORDER",8,	
	ifelse (ETC$KEYWORDVAR=="AEF99 MOVE",8,	
	ifelse (ETC$KEYWORDVAR=="ACR99 SOLIDWORKS",8,	
	ifelse (ETC$KEYWORDVAR=="AAB09 SAP PRINT",8,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="ACV02 FILE DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AGF99 DESKTOP",8,	
	ifelse (ETC$KEYWORDVAR=="ACC99 EXCEL",8,	
	ifelse (ETC$KEYWORDVAR=="AGA99 BATTERY",8,	
	ifelse (ETC$KEYWORDVAR=="AHM99 LICENSE",8,	
	ifelse (ETC$KEYWORDVAR=="AIL99 MAT",8,	
	ifelse (ETC$KEYWORDVAR=="ABL99 ASTEA",8,	
	ifelse (ETC$KEYWORDVAR=="ABX99 ADP",8,	
	ifelse (ETC$KEYWORDVAR=="ACF03 INTRANET ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ACW99 SYSTEM",8,	
	ifelse (ETC$KEYWORDVAR=="ADO99 VOIP",8,	
	ifelse (ETC$KEYWORDVAR=="AAB16 SAP CHANGE",8,	
	ifelse (ETC$KEYWORDVAR=="ADL99 ITCHECK",8,	
	ifelse (ETC$KEYWORDVAR=="ACZ15 USERS ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AHO99 OUTPUT",8,	
	ifelse (ETC$KEYWORDVAR=="AAE10 EMAIL PHONE",8,	
	ifelse (ETC$KEYWORDVAR=="AEH99 MONITOR",8,	
	ifelse (ETC$KEYWORDVAR=="AFS99 INK",8,	
	ifelse (ETC$KEYWORDVAR=="ACE99 EXCHANGE",8,	
	ifelse (ETC$KEYWORDVAR=="AIE99 UPLOAD",8,	
	ifelse (ETC$KEYWORDVAR=="ABH99 EPDM",8,	
	ifelse (ETC$KEYWORDVAR=="ADA08 NEW ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AES99 WG",8,	
	ifelse (ETC$KEYWORDVAR=="AFH99 FIREWALL",8,	
	ifelse (ETC$KEYWORDVAR=="AAK19 INSTALL ALTIUM",8,	
	ifelse (ETC$KEYWORDVAR=="AAB06 SAP NEW REQUEST",8,	
	ifelse (ETC$KEYWORDVAR=="ABF01 SFDC DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AAS05 SWITCH ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ADI99 SNC",8,	
	ifelse (ETC$KEYWORDVAR=="AAB07 SAP NEW ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="ADX02 WIRELESS DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AEN99 SPACE",8,	
	ifelse (ETC$KEYWORDVAR=="AIK99 LOG",8,	
	ifelse (ETC$KEYWORDVAR=="AAX01 EQUIPMENT FOLLOWUP",8,	
	ifelse (ETC$KEYWORDVAR=="ABG01 FIS ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAK13 INSTALL SCAN",8,	
	ifelse (ETC$KEYWORDVAR=="ACU03 DRIVE ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ABG02 FIS DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AAG06 PRINT TONER",8,	
	ifelse (ETC$KEYWORDVAR=="ACY03 APP DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ADB02 PHONE ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ADX99 WIRELESS",8,	
	ifelse (ETC$KEYWORDVAR=="AAP09 SERVER ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="ABM01 BW ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ABH03 EPDM ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AEI99 MOUSE",8,	
	ifelse (ETC$KEYWORDVAR=="AFC99 QUOTE",8,	
	ifelse (ETC$KEYWORDVAR=="AFZ99 ADDRESS",8,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="AHA99 DATA",8,	
	ifelse (ETC$KEYWORDVAR=="ADT99 INTERFACE",8,	
	ifelse (ETC$KEYWORDVAR=="ABR01 SHAREPOINT ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AIS99 MAINTENANCE",8,	
	ifelse (ETC$KEYWORDVAR=="AGX99 ENERGY",8,	
	ifelse (ETC$KEYWORDVAR=="AHD99 ALERT",8,	
	ifelse (ETC$KEYWORDVAR=="AHE99 CHART",8,	
	ifelse (ETC$KEYWORDVAR=="AIN99 CAR",8,	
	ifelse (ETC$KEYWORDVAR=="ACX13 SERVICE ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ADE04 SCAN COMPUTER",8,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP CODE",8,	
	ifelse (ETC$KEYWORDVAR=="AAF12 MAIL CHANGE",8,	
	ifelse (ETC$KEYWORDVAR=="AAG09 FIX PRINTER",8,	
	ifelse (ETC$KEYWORDVAR=="AAV05 STATION COMPUTER",8,	
	ifelse (ETC$KEYWORDVAR=="ABG03 FIS ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AEX99 SHIP",8,	
	ifelse (ETC$KEYWORDVAR=="AAK23 INSTALL DRIVE",8,	
	ifelse (ETC$KEYWORDVAR=="ACP03 SITE ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAG07 PRINT INK",8,	
	ifelse (ETC$KEYWORDVAR=="AER99 DASHBOARD",8,	
	ifelse (ETC$KEYWORDVAR=="AAK17 INSTALL BW",8,	
	ifelse (ETC$KEYWORDVAR=="AAI02 BLOCK SITE",8,	
	ifelse (ETC$KEYWORDVAR=="ACW02 SYSTEM ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAL05 SETUP OUTLOOK",8,	
	ifelse (ETC$KEYWORDVAR=="AGH03 TIME ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ADB04 FIX PHONE",8,	
	ifelse (ETC$KEYWORDVAR=="ADV03 PORTAL ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ABP99 ALTIUM",8,	
	ifelse (ETC$KEYWORDVAR=="AAE13 EMAIL RESTORE",8,	
	ifelse (ETC$KEYWORDVAR=="AAH49 LOGIN ISSUE SFORCE",8,	
	ifelse (ETC$KEYWORDVAR=="AAH51 LOGIN ISSUE TOMCAT",8,	
	ifelse (ETC$KEYWORDVAR=="ABN99 BOM",8,	
	ifelse (ETC$KEYWORDVAR=="AAG14 PRINT JOB",8,	
	ifelse (ETC$KEYWORDVAR=="AAH50 LOGIN ISSUE SFDC",8,	
	ifelse (ETC$KEYWORDVAR=="ACM02 SQL DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AAE19 EMAIL JUNK",8,	
	ifelse (ETC$KEYWORDVAR=="AAP04 SERVER BACKUP",8,	
	ifelse (ETC$KEYWORDVAR=="AAJ06 LOCK FIS",8,	
	ifelse (ETC$KEYWORDVAR=="AAL35 SETUP SOFTWARE",8,	
	ifelse (ETC$KEYWORDVAR=="AAA42 PASSWORD FIS",8,	
	ifelse (ETC$KEYWORDVAR=="ABK02 REFUSOL DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW SNC",8,	
	ifelse (ETC$KEYWORDVAR=="AAV04 STATION ADMIN",8,	
	ifelse (ETC$KEYWORDVAR=="ADU99 QUEUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAA34 PASSWORD ITCHECK",8,	
	ifelse (ETC$KEYWORDVAR=="AAU01 LINK ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ADD09 FAX ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAB23 SAP EXCEL",8,	paste(ETC$KEYWORDVAR_R2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R2 <-	ifelse (ETC$KEYWORDVAR=="ADH01 DEBUG REQ",8,	
	ifelse (ETC$KEYWORDVAR=="AAS01 SWITCH ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ADD02 FAX DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ADB06 PHONE ADMIN",8,	
	ifelse (ETC$KEYWORDVAR=="AAL18 SETUP SKYPE",8,	
	ifelse (ETC$KEYWORDVAR=="AFI99 BARCODE",8,	
	ifelse (ETC$KEYWORDVAR=="AAH40 LOGIN ISSUE FIS",8,	
	ifelse (ETC$KEYWORDVAR=="AAR99 ROUTER",8,	
	ifelse (ETC$KEYWORDVAR=="AAP08 SERVER ROLLOUT",8,	
	ifelse (ETC$KEYWORDVAR=="ADE01 SCAN ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ACY09 APP ROLLOUT",8,	
	ifelse (ETC$KEYWORDVAR=="AAL09 SETUP ADMIN",8,	paste(ETC$KEYWORDVAR_R2)))))))))))))
	
ETC$KEYWORDVAR_R2 <- ifelse (ETC$KEYWORDVAR_R2 == "NA", 6, paste(ETC$KEYWORDVAR_R2))

ETC$KEYWORDVAR_O3 <- "NA"

		
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAA15 PASSWORD RESET PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK29 INSTALL EXPENS",2,	
	ifelse (ETC$KEYWORDVAR=="AAL25 SETUP INTERNATIONAL",3,	
	ifelse (ETC$KEYWORDVAR=="AAM05 CONFIG PROJECT",4,	
	ifelse (ETC$KEYWORDVAR=="AAW99 VM",5,	
	ifelse (ETC$KEYWORDVAR=="ADD02 FAX DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="AFP99 FTP",7,	
	ifelse (ETC$KEYWORDVAR=="AAA11 PASSWORD RESET COMPUTER",8,	
	ifelse (ETC$KEYWORDVAR=="AAA13 PASSWORD RESET WINDOWS",9,	
	ifelse (ETC$KEYWORDVAR=="AAB35 SAP TS3",10,	
	ifelse (ETC$KEYWORDVAR=="AAC14 WEBEX TOOL",11,	
	ifelse (ETC$KEYWORDVAR=="AAL01 SETUP EPDM",12,	
	ifelse (ETC$KEYWORDVAR=="AAL10 SETUP ADOBE",13,	
	ifelse (ETC$KEYWORDVAR=="AAM01 CONFIG OUTLOOK",14,	
	ifelse (ETC$KEYWORDVAR=="ACF02 INTRANET DOWN NOTWORK",15,	
	ifelse (ETC$KEYWORDVAR=="ACX07 SERVICE VIRUS HACK",16,	
	ifelse (ETC$KEYWORDVAR=="ACZ17 USERS ISSUE",17,	
	ifelse (ETC$KEYWORDVAR=="ADD09 FAX ISSUE",18,	
	ifelse (ETC$KEYWORDVAR=="AGW99 QUICKEN",19,	
	ifelse (ETC$KEYWORDVAR=="AAA47 PASSWORD RESET AUTO REPLY",20,	
	ifelse (ETC$KEYWORDVAR=="AAG13 PRINT CANCEL",21,	
	ifelse (ETC$KEYWORDVAR=="ACC02 EXCEL DOWN NOTWORK",22,	
	ifelse (ETC$KEYWORDVAR=="ADX03 WIRELESS ISSUE",23,	
	ifelse (ETC$KEYWORDVAR=="AGL99 MALWARE",24,	
	ifelse (ETC$KEYWORDVAR=="AAE09 EMAIL SCAN",25,	
	ifelse (ETC$KEYWORDVAR=="AAE15 EMAIL AUTH",26,	
	ifelse (ETC$KEYWORDVAR=="AAK09 INSTALL GOOGLE",27,	
	ifelse (ETC$KEYWORDVAR=="AAK31 INSTALL OS",28,	
	ifelse (ETC$KEYWORDVAR=="AAL08 SETUP FAX",29,	
	ifelse (ETC$KEYWORDVAR=="AAM04 CONFIG PHONE",30,	
	ifelse (ETC$KEYWORDVAR=="AAO02 VPN DOWN NOTWORK",31,	
	ifelse (ETC$KEYWORDVAR=="ABJ01 TS3 ACESS CONNECT",32,	
	ifelse (ETC$KEYWORDVAR=="AAA16 PASSWORD PHONE",33,	
	ifelse (ETC$KEYWORDVAR=="AAK03 INSTALL FIS",34,	
	ifelse (ETC$KEYWORDVAR=="AAL11 SETUP ALTIUM",35,	
	ifelse (ETC$KEYWORDVAR=="AAL22 SETUP STATION",36,	
	ifelse (ETC$KEYWORDVAR=="ABM02 BW DOWN NOTWORK",37,	
	ifelse (ETC$KEYWORDVAR=="ABO99 FUSIONOPS",38,	
	ifelse (ETC$KEYWORDVAR=="ACO02 WEBSITE DOWN NOTWORK",39,	
	ifelse (ETC$KEYWORDVAR=="AGJ99 ANONYMOUS",40,	
	ifelse (ETC$KEYWORDVAR=="AAB39 SAP TICKET",41,	
	ifelse (ETC$KEYWORDVAR=="AAC10 WEBEX SETTING",42,	
	ifelse (ETC$KEYWORDVAR=="AAF05 MAIL INSTALL",43,	
	ifelse (ETC$KEYWORDVAR=="AAR02 ROUTER DOWN NOTWORK",44,	
	ifelse (ETC$KEYWORDVAR=="AAS05 SWITCH ISSUE",45,	
	ifelse (ETC$KEYWORDVAR=="ABE01 TOOL DOWN NOTWORK",46,	
	ifelse (ETC$KEYWORDVAR=="ABZ02 SALESFORCE DOWN NOTWORK",47,	
	ifelse (ETC$KEYWORDVAR=="ADY01 WIFI ACESS CONNECT",48,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAA36 PASSWORD PCARD",49,	
	ifelse (ETC$KEYWORDVAR=="AAK10 INSTALL OUTLOOK",50,	
	ifelse (ETC$KEYWORDVAR=="AAK13 INSTALL SCAN",51,	
	ifelse (ETC$KEYWORDVAR=="AAR04 ROUTER ISSUE",52,	
	ifelse (ETC$KEYWORDVAR=="AAV03 STATION ACCOUNT",53,	
	ifelse (ETC$KEYWORDVAR=="ABP03 ALTIUM ISSUE",54,	
	ifelse (ETC$KEYWORDVAR=="AHV99 RESET",55,	
	ifelse (ETC$KEYWORDVAR=="AAE19 EMAIL JUNK",56,	
	ifelse (ETC$KEYWORDVAR=="AAO03 VPN ISSUE",57,	
	ifelse (ETC$KEYWORDVAR=="ADN99 CPU",58,	
	ifelse (ETC$KEYWORDVAR=="ADW01 DOMAIN ACESS CONNECT",59,	
	ifelse (ETC$KEYWORDVAR=="AFR99 TONER",60,	
	ifelse (ETC$KEYWORDVAR=="AAC01 WEBEX DISABLE",61,	
	ifelse (ETC$KEYWORDVAR=="AAF10 MAIL PHONE",62,	
	ifelse (ETC$KEYWORDVAR=="AAL17 SETUP PDF",63,	
	ifelse (ETC$KEYWORDVAR=="ACF99 INTRANET",64,	
	ifelse (ETC$KEYWORDVAR=="ACV01 FILE ACESS CONNECT",65,	
	ifelse (ETC$KEYWORDVAR=="AGH01 TIME ACESS CONNECT",66,	
	ifelse (ETC$KEYWORDVAR=="AAH48 LOGIN ISSUE REMOTE",67,	
	ifelse (ETC$KEYWORDVAR=="AAK05 INSTALL CHINESE",68,	
	ifelse (ETC$KEYWORDVAR=="ACB01 OFFICE ACESS CONNECT",69,	
	ifelse (ETC$KEYWORDVAR=="AHS99 PRODUCT",70,	
	ifelse (ETC$KEYWORDVAR=="AAG04 PRINT LOGIN",71,	
	ifelse (ETC$KEYWORDVAR=="AAL16 SETUP DRIVE",72,	
	ifelse (ETC$KEYWORDVAR=="AFI99 BARCODE",73,	
	ifelse (ETC$KEYWORDVAR=="AAA10 PASSWORD WEBEX",74,	
	ifelse (ETC$KEYWORDVAR=="AAL04 SETUP WINDOWS",75,	
	ifelse (ETC$KEYWORDVAR=="AAA02 PASSWORD EMAIL",76,	
	ifelse (ETC$KEYWORDVAR=="AAA48 PASSWORD RESET AUTO",77,	
	ifelse (ETC$KEYWORDVAR=="AAP05 SERVER RESTORE",78,	
	ifelse (ETC$KEYWORDVAR=="AAP08 SERVER ROLLOUT",79,	
	ifelse (ETC$KEYWORDVAR=="AAW02 VM DOWN NOTWORK",80,	
	ifelse (ETC$KEYWORDVAR=="AAF20 MAIL OUTLOOK",81,	
	ifelse (ETC$KEYWORDVAR=="AGG02 PC DOWN NOTWORK",82,	
	ifelse (ETC$KEYWORDVAR=="AAG07 PRINT INK",83,	
	ifelse (ETC$KEYWORDVAR=="AAK30 INSTALL IE",84,	
	ifelse (ETC$KEYWORDVAR=="ABL02 ASTEA DOWN NOTWORK",85,	
	ifelse (ETC$KEYWORDVAR=="ACC99 EXCEL",86,	
	ifelse (ETC$KEYWORDVAR=="ACT02 FOLDER DOWN NOTWORK",87,	
	ifelse (ETC$KEYWORDVAR=="AGF02 DESKTOP DOWN NOTWORK",88,	
	ifelse (ETC$KEYWORDVAR=="AAB24 SAP PART",89,	
	ifelse (ETC$KEYWORDVAR=="AAF25 MAIL FULL",90,	
	ifelse (ETC$KEYWORDVAR=="AAD01 VOICEMAIL DISABLE",91,	
	ifelse (ETC$KEYWORDVAR=="AAG09 FIX PRINTER",92,	
	ifelse (ETC$KEYWORDVAR=="ABP99 ALTIUM",93,	
	ifelse (ETC$KEYWORDVAR=="ACD02 OUTLOOK DOWN NOTWORK",94,	
	ifelse (ETC$KEYWORDVAR=="ACM01 SQL ACESS CONNECT",95,	
	ifelse (ETC$KEYWORDVAR=="ADB04 FIX PHONE",96,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAF12 MAIL CHANGE",97,	
	ifelse (ETC$KEYWORDVAR=="AGC99 MACHINE",98,	
	ifelse (ETC$KEYWORDVAR=="AIQ99 TRAINING",99,	
	ifelse (ETC$KEYWORDVAR=="AAL09 SETUP ADMIN",100,	
	ifelse (ETC$KEYWORDVAR=="ACZ20 USER DOWN NOTWORK",101,	
	ifelse (ETC$KEYWORDVAR=="AIM99 CART",102,	
	ifelse (ETC$KEYWORDVAR=="AAQ14 NETWORK ISSUE",103,	
	ifelse (ETC$KEYWORDVAR=="AAE03 EMAIL DOWN NOTWORK",104,	
	ifelse (ETC$KEYWORDVAR=="AAJ04 LOCK COMPUTER",105,	
	ifelse (ETC$KEYWORDVAR=="ACF01 INTRANET ACESS CONNECT",106,	
	ifelse (ETC$KEYWORDVAR=="ADW02 DOMAIN DOWN NOTWORK",107,	
	ifelse (ETC$KEYWORDVAR=="AAE10 EMAIL PHONE",108,	
	ifelse (ETC$KEYWORDVAR=="AAE16 EMAIL GROUP",109,	
	ifelse (ETC$KEYWORDVAR=="AIF99 UPS",110,	
	ifelse (ETC$KEYWORDVAR=="AAK33 INSTALL SOFTWARE",111,	
	ifelse (ETC$KEYWORDVAR=="AAL20 SETUP WIRELESS",112,	
	ifelse (ETC$KEYWORDVAR=="AAT03 NODE REBOOT",113,	
	ifelse (ETC$KEYWORDVAR=="ABJ03 TS3 ISSUE",114,	
	ifelse (ETC$KEYWORDVAR=="ABL01 ASTEA ACESS CONNECT",115,	
	ifelse (ETC$KEYWORDVAR=="ACW13 SYSTEM ISSUE",116,	
	ifelse (ETC$KEYWORDVAR=="AHP99 PACK",117,	
	ifelse (ETC$KEYWORDVAR=="AAC17 WEBEX CONNECT",118,	
	ifelse (ETC$KEYWORDVAR=="AAG02 PRINT DOWN NOTWORK PROD",119,	
	ifelse (ETC$KEYWORDVAR=="AAH31 LOGIN ISSUE BW",120,	
	ifelse (ETC$KEYWORDVAR=="AAL26 SETUP TECHNICIAN",121,	
	ifelse (ETC$KEYWORDVAR=="ABI03 PLM ISSUE",122,	
	ifelse (ETC$KEYWORDVAR=="ACP01 SITE ACESS CONNECT",123,	
	ifelse (ETC$KEYWORDVAR=="ADX99 WIRELESS",124,	
	ifelse (ETC$KEYWORDVAR=="AEI99 MOUSE",125,	
	ifelse (ETC$KEYWORDVAR=="AGF03 DESKTOP ISSUE",126,	
	ifelse (ETC$KEYWORDVAR=="AAE04 EMAIL LOGIN",127,	
	ifelse (ETC$KEYWORDVAR=="AAK23 INSTALL DRIVE",128,	
	ifelse (ETC$KEYWORDVAR=="AAL28 SETUP SFDC",129,	
	ifelse (ETC$KEYWORDVAR=="ABI99 PLM",130,	
	ifelse (ETC$KEYWORDVAR=="AAB07 SAP NEW ACCOUNT",131,	
	ifelse (ETC$KEYWORDVAR=="AAG11 PRINT AUTO",132,	
	ifelse (ETC$KEYWORDVAR=="AAH24 LOGIN NOT WORK",133,	
	ifelse (ETC$KEYWORDVAR=="ADB09 PHONE DELIVERY",134,	
	ifelse (ETC$KEYWORDVAR=="AAL05 SETUP OUTLOOK",135,	
	ifelse (ETC$KEYWORDVAR=="ABN03 BOM ISSUE",136,	
	ifelse (ETC$KEYWORDVAR=="AFM99 LIST",137,	
	ifelse (ETC$KEYWORDVAR=="AAD99 VOICEMAIL",138,	
	ifelse (ETC$KEYWORDVAR=="AAH03 LOGIN NOT WORK COMPUTER",139,	
	ifelse (ETC$KEYWORDVAR=="AAZ01 JOB STEP1",140,	
	ifelse (ETC$KEYWORDVAR=="ACX11 SERVICE ADMIN",141,	
	ifelse (ETC$KEYWORDVAR=="AAH99 LOGIN",142,	
	ifelse (ETC$KEYWORDVAR=="AAL30 SETUP SERVRER",143,	
	ifelse (ETC$KEYWORDVAR=="AAP01 SERVER ACESS CONNECT",144,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="ADT99 INTERFACE",145,	
	ifelse (ETC$KEYWORDVAR=="AHN99 MESSAGE",146,	
	ifelse (ETC$KEYWORDVAR=="AAB06 SAP NEW REQUEST",147,	
	ifelse (ETC$KEYWORDVAR=="AAB21 SAP GROUP",148,	
	ifelse (ETC$KEYWORDVAR=="AAB32 SAP PORTAL",149,	
	ifelse (ETC$KEYWORDVAR=="AAF03 MAIL DOWN NOTWORK",150,	
	ifelse (ETC$KEYWORDVAR=="AAL23 SETUP TOOL",151,	
	ifelse (ETC$KEYWORDVAR=="ABU99 ADOBE",152,	
	ifelse (ETC$KEYWORDVAR=="ACX10 SERVICE ACCOUNT",153,	
	ifelse (ETC$KEYWORDVAR=="ADV03 PORTAL ISSUE",154,	
	ifelse (ETC$KEYWORDVAR=="AAJ99 LOCK",155,	
	ifelse (ETC$KEYWORDVAR=="AAK15 INSTALL ADMIN",156,	
	ifelse (ETC$KEYWORDVAR=="AAK19 INSTALL ALTIUM",157,	
	ifelse (ETC$KEYWORDVAR=="ABD01 PTOOL DOWN NOTWORK",158,	
	ifelse (ETC$KEYWORDVAR=="AIE99 UPLOAD",159,	
	ifelse (ETC$KEYWORDVAR=="AAC05 WEBEX INSTALL",160,	
	ifelse (ETC$KEYWORDVAR=="ABW99 ATS",161,	
	ifelse (ETC$KEYWORDVAR=="ACE02 EXCHANGE DOWN NOTWORK",162,	
	ifelse (ETC$KEYWORDVAR=="ACG99 EXTRANET",163,	
	ifelse (ETC$KEYWORDVAR=="ADB08 PHONE CHANGE",164,	
	ifelse (ETC$KEYWORDVAR=="AFQ99 EFT",165,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP INSTALL",166,	
	ifelse (ETC$KEYWORDVAR=="AAC04 WEBEX LOGIN",167,	
	ifelse (ETC$KEYWORDVAR=="AAI02 BLOCK SITE",168,	
	ifelse (ETC$KEYWORDVAR=="AAP10 SERVER ADMIN",169,	
	ifelse (ETC$KEYWORDVAR=="ABG01 FIS ACESS CONNECT",170,	
	ifelse (ETC$KEYWORDVAR=="ADV01 PORTAL ACESS CONNECT",171,	
	ifelse (ETC$KEYWORDVAR=="AAZ02 JOB DOWN NOTWORK",172,	
	ifelse (ETC$KEYWORDVAR=="ACO01 WEBSITE ACESS CONNECT",173,	
	ifelse (ETC$KEYWORDVAR=="AHM99 LICENSE",174,	
	ifelse (ETC$KEYWORDVAR=="AAE26 EMAIL REQUEST",175,	
	ifelse (ETC$KEYWORDVAR=="AAN02 INTERNET DOWN NOTWORK",176,	
	ifelse (ETC$KEYWORDVAR=="ADE02 SCAN DOWN NOTWORK",177,	
	ifelse (ETC$KEYWORDVAR=="ADR99 SOFTWARE",178,	
	ifelse (ETC$KEYWORDVAR=="ADX02 WIRELESS DOWN NOTWORK",179,	
	ifelse (ETC$KEYWORDVAR=="AFS99 INK",180,	
	ifelse (ETC$KEYWORDVAR=="AAK26 INSTALL PDF",181,	
	ifelse (ETC$KEYWORDVAR=="AAP04 SERVER BACKUP",182,	
	ifelse (ETC$KEYWORDVAR=="ABF02 SFDC ISSUE",183,	
	ifelse (ETC$KEYWORDVAR=="ACY11 APP ADMIN",184,	
	ifelse (ETC$KEYWORDVAR=="ADM99 FLASH",185,	
	ifelse (ETC$KEYWORDVAR=="AAC99 WEBEX",186,	
	ifelse (ETC$KEYWORDVAR=="AAK02 INSTALL EPDM",187,	
	ifelse (ETC$KEYWORDVAR=="AGX99 ENERGY",188,	
	ifelse (ETC$KEYWORDVAR=="AAU01 LINK ACESS CONNECT",189,	
	ifelse (ETC$KEYWORDVAR=="ACW10 SYSTEM ACCOUNT",190,	
	ifelse (ETC$KEYWORDVAR=="AEC99 IPAD IPHONE",191,	
	ifelse (ETC$KEYWORDVAR=="AFN99 ROLE",192,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAB33 SAP REFUSOL",193,	
	ifelse (ETC$KEYWORDVAR=="AAE20 EMAIL OUTLOOK",194,	
	ifelse (ETC$KEYWORDVAR=="ABH99 EPDM",195,	
	ifelse (ETC$KEYWORDVAR=="AGF99 DESKTOP",196,	
	ifelse (ETC$KEYWORDVAR=="AAA39 PASSWORD RESET TS3",197,	
	ifelse (ETC$KEYWORDVAR=="AAC06 WEBEX ACCOUNT",198,	
	ifelse (ETC$KEYWORDVAR=="ADB01 PHONE COMPANY",199,	
	ifelse (ETC$KEYWORDVAR=="AEU03 PORT ISSUE",200,	
	ifelse (ETC$KEYWORDVAR=="AAA07 PASSWORD RESET SAP",201,	
	ifelse (ETC$KEYWORDVAR=="ABE02 TOOL ISSUE",202,	
	ifelse (ETC$KEYWORDVAR=="ACK03 CAD ISSUE",203,	
	ifelse (ETC$KEYWORDVAR=="ACZ12 NEW HIRE ACCOUNT",204,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW PHONE",205,	
	ifelse (ETC$KEYWORDVAR=="AIN99 CAR",206,	
	ifelse (ETC$KEYWORDVAR=="AAA99 PASSWORD",207,	
	ifelse (ETC$KEYWORDVAR=="AAB14 SAP MATERIAL",208,	
	ifelse (ETC$KEYWORDVAR=="AAN01 INTERNET ACESS CONNECT",209,	
	ifelse (ETC$KEYWORDVAR=="ABH02 EPDM DOWN NOTWORK",210,	
	ifelse (ETC$KEYWORDVAR=="ACW99 SYSTEM",211,	
	ifelse (ETC$KEYWORDVAR=="AAE21 EMAIL TOOL",212,	
	ifelse (ETC$KEYWORDVAR=="AAF04 MAIL LOGIN",213,	
	ifelse (ETC$KEYWORDVAR=="AAG20 PRINT ISSUE",214,	
	ifelse (ETC$KEYWORDVAR=="AAK99 INSTALL",215,	
	ifelse (ETC$KEYWORDVAR=="ACU99 DRIVE",216,	
	ifelse (ETC$KEYWORDVAR=="ACW01 SYSTEM UNATHOR ACCESS",217,	
	ifelse (ETC$KEYWORDVAR=="AFO99 BACKUP",218,	
	ifelse (ETC$KEYWORDVAR=="AHO99 OUTPUT",219,	
	ifelse (ETC$KEYWORDVAR=="AAH12 LOGIN NOT WORK INTERNET",220,	
	ifelse (ETC$KEYWORDVAR=="AAH23 LOGIN NOT WORK VPN",221,	
	ifelse (ETC$KEYWORDVAR=="AAL12 SETUP PHONE",222,	
	ifelse (ETC$KEYWORDVAR=="ABZ99 SALESFORCE",223,	
	ifelse (ETC$KEYWORDVAR=="AAI99 BLOCK",224,	
	ifelse (ETC$KEYWORDVAR=="AAK16 INSTALL CAD",225,	
	ifelse (ETC$KEYWORDVAR=="AAP07 SERVER FILE",226,	
	ifelse (ETC$KEYWORDVAR=="AAX01 EQUIPMENT FOLLOWUP",227,	
	ifelse (ETC$KEYWORDVAR=="ACX03 SERVICE DOWN NOTWORK",228,	
	ifelse (ETC$KEYWORDVAR=="ADB11 PHONE DIRECTORY",229,	
	ifelse (ETC$KEYWORDVAR=="AFL99 CASE",230,	
	ifelse (ETC$KEYWORDVAR=="AGD01 COMPUTER ACESS CONNECT",231,	
	ifelse (ETC$KEYWORDVAR=="AAN99 INTERNET",232,	
	ifelse (ETC$KEYWORDVAR=="ABD02 PTOOL ISSUE",233,	
	ifelse (ETC$KEYWORDVAR=="ADG01 CRITICAL ALERT",234,	
	ifelse (ETC$KEYWORDVAR=="AAH33 LOGIN ISSUE COMPUTER",235,	
	ifelse (ETC$KEYWORDVAR=="AIR99 TEST",236,	
	ifelse (ETC$KEYWORDVAR=="AAB09 SAP PRINT",237,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP CODE",238,	
	ifelse (ETC$KEYWORDVAR=="AAK01 INSTALL PLM",239,	
	ifelse (ETC$KEYWORDVAR=="AAK11 INSTALL OFFICE",240,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAQ01 NETWORK WIRELESS",241,	
	ifelse (ETC$KEYWORDVAR=="ACD99 OUTLOOK",242,	
	ifelse (ETC$KEYWORDVAR=="ACZ11 NEW HIRE CODE",243,	
	ifelse (ETC$KEYWORDVAR=="ACD01 OUTLOOK ACESS CONNECT",244,	
	ifelse (ETC$KEYWORDVAR=="ACZ14 NEW HIRE",245,	
	ifelse (ETC$KEYWORDVAR=="ADX01 WIRELESS ACESS CONNECT",246,	
	ifelse (ETC$KEYWORDVAR=="AEN99 SPACE",247,	
	ifelse (ETC$KEYWORDVAR=="AFG99 DELIVER",248,	
	ifelse (ETC$KEYWORDVAR=="AAI01 BLOCK VENDER",249,	
	ifelse (ETC$KEYWORDVAR=="AAQ10 NETWORK COMPUTER",250,	
	ifelse (ETC$KEYWORDVAR=="AAY99 INVENTORY",251,	
	ifelse (ETC$KEYWORDVAR=="ABK01 REFUSOL ACESS CONNECT",252,	
	ifelse (ETC$KEYWORDVAR=="ABS03 PROJECT ISSUE",253,	
	ifelse (ETC$KEYWORDVAR=="ACH02 DELL DOWN NOTWORK",254,	
	ifelse (ETC$KEYWORDVAR=="ACT99 FOLDER",255,	
	ifelse (ETC$KEYWORDVAR=="AGQ99 ADMIN",256,	
	ifelse (ETC$KEYWORDVAR=="AAB12 SAP INVENTORY",257,	
	ifelse (ETC$KEYWORDVAR=="ABH01 EPDM ACESS CONNECT",258,	
	ifelse (ETC$KEYWORDVAR=="AAO01 VPN ACESS CONNECT",259,	
	ifelse (ETC$KEYWORDVAR=="AAP06 SERVER VIRUS HACK",260,	
	ifelse (ETC$KEYWORDVAR=="ABV99 GOOGLE",261,	
	ifelse (ETC$KEYWORDVAR=="AAB17 SAP AUTH",262,	
	ifelse (ETC$KEYWORDVAR=="AAG05 PRINT INSTALL",263,	
	ifelse (ETC$KEYWORDVAR=="ABQ03 ENGINEER ISSUE",264,	
	ifelse (ETC$KEYWORDVAR=="AAJ10 LOCK ACCOUNT",265,	
	ifelse (ETC$KEYWORDVAR=="AAN03 INTERNET ISSUE",266,	
	ifelse (ETC$KEYWORDVAR=="AEO99 VOLUME",267,	
	ifelse (ETC$KEYWORDVAR=="AGB99 POWER",268,	
	ifelse (ETC$KEYWORDVAR=="AHT99 RECORD",269,	
	ifelse (ETC$KEYWORDVAR=="AAA08 PASSWORD SAP",270,	
	ifelse (ETC$KEYWORDVAR=="AAC12 WEBEX MEETING",271,	
	ifelse (ETC$KEYWORDVAR=="AAU99 LINK",272,	
	ifelse (ETC$KEYWORDVAR=="AHF99 DIRECTORY",273,	
	ifelse (ETC$KEYWORDVAR=="AAB08 SAP ACCOUNT",274,	
	ifelse (ETC$KEYWORDVAR=="AAC15 WEBEX ISSUE",275,	
	ifelse (ETC$KEYWORDVAR=="ABZ01 SALESFORCE ACESS CONNECT",276,	
	ifelse (ETC$KEYWORDVAR=="ACM02 SQL DOWN NOTWORK",277,	
	ifelse (ETC$KEYWORDVAR=="ACX02 SERVICE ACESS CONNECT",278,	
	ifelse (ETC$KEYWORDVAR=="ACC03 EXCEL ISSUE",279,	
	ifelse (ETC$KEYWORDVAR=="AAK22 INSTALL DELL",280,	
	ifelse (ETC$KEYWORDVAR=="AAP99 SERVER",281,	
	ifelse (ETC$KEYWORDVAR=="AAQ02 NETWORK ACESS CONNECT",282,	
	ifelse (ETC$KEYWORDVAR=="ACU03 DRIVE ISSUE",283,	
	ifelse (ETC$KEYWORDVAR=="AAK28 INSTALL SQL",284,	
	ifelse (ETC$KEYWORDVAR=="AAV99 STATION",285,	
	ifelse (ETC$KEYWORDVAR=="AEA99 LAN",286,	
	ifelse (ETC$KEYWORDVAR=="AEU02 PORT DOWN NOTWORK",287,	
	ifelse (ETC$KEYWORDVAR=="AIA99 SPYWARE",288,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="ABR03 SHAREPOINT ISSUE",289,	
	ifelse (ETC$KEYWORDVAR=="ACY10 APP ACCOUNT",290,	
	ifelse (ETC$KEYWORDVAR=="ACZ21 USER ISSUE",291,	
	ifelse (ETC$KEYWORDVAR=="AES99 WG",292,	
	ifelse (ETC$KEYWORDVAR=="AGH03 TIME ISSUE",293,	
	ifelse (ETC$KEYWORDVAR=="AAS02 SWITCH DOWN NOTWORK",294,	
	ifelse (ETC$KEYWORDVAR=="ACW03 SYSTEM DOWN NOTWORK",295,	
	ifelse (ETC$KEYWORDVAR=="ADF01 INCIDENT ATTACK",296,	
	ifelse (ETC$KEYWORDVAR=="AAA30 PASSWORD ASTEA",297,	
	ifelse (ETC$KEYWORDVAR=="AAL99 SETUP",298,	
	ifelse (ETC$KEYWORDVAR=="AAV05 STATION COMPUTER",299,	
	ifelse (ETC$KEYWORDVAR=="ACU01 DRIVE ACESS CONNECT",300,	
	ifelse (ETC$KEYWORDVAR=="AAB01 SAP ACESS CONNECT",301,	
	ifelse (ETC$KEYWORDVAR=="ABR99 SHAREPOINT",302,	
	ifelse (ETC$KEYWORDVAR=="ACB99 OFFICE",303,	
	ifelse (ETC$KEYWORDVAR=="AGO99 DOWN",304,	
	ifelse (ETC$KEYWORDVAR=="AAB41 SAP REQUEST",305,	
	ifelse (ETC$KEYWORDVAR=="AAG03 PRINT DOWN NOTWORK",306,	
	ifelse (ETC$KEYWORDVAR=="ABG02 FIS DOWN NOTWORK",307,	
	ifelse (ETC$KEYWORDVAR=="AFB99 PURCHASE",308,	
	ifelse (ETC$KEYWORDVAR=="ACT01 FOLDER ACESS CONNECT",309,	
	ifelse (ETC$KEYWORDVAR=="AAK07 INSTALL WINDOWS",310,	
	ifelse (ETC$KEYWORDVAR=="ACK02 CAD DOWN NOTWORK",311,	
	ifelse (ETC$KEYWORDVAR=="ADA07 NEW COMPUTER",312,	
	ifelse (ETC$KEYWORDVAR=="AAM06 CONFIG FIS",313,	
	ifelse (ETC$KEYWORDVAR=="AAW05 VM COMPUTER",314,	
	ifelse (ETC$KEYWORDVAR=="ACT03 FOLDER ISSUE",315,	
	ifelse (ETC$KEYWORDVAR=="AEF99 MOVE",316,	
	ifelse (ETC$KEYWORDVAR=="AGP99 ACCESS",317,	
	ifelse (ETC$KEYWORDVAR=="AAC08 WEBEX CHANGE",318,	
	ifelse (ETC$KEYWORDVAR=="ACY03 APP DOWN NOTWORK",319,	
	ifelse (ETC$KEYWORDVAR=="AGG99 PC",320,	
	ifelse (ETC$KEYWORDVAR=="AGT99 ISSUE",321,	
	ifelse (ETC$KEYWORDVAR=="AIC99 TRANSACTION",322,	
	ifelse (ETC$KEYWORDVAR=="AAF07 MAIL ACCOUNT",323,	
	ifelse (ETC$KEYWORDVAR=="ADW99 DOMAIN",324,	
	ifelse (ETC$KEYWORDVAR=="AFK99 GROUP",325,	
	ifelse (ETC$KEYWORDVAR=="AFZ99 ADDRESS",326,	
	ifelse (ETC$KEYWORDVAR=="ACZ16 USERS DOWN NOTWORK",327,	
	ifelse (ETC$KEYWORDVAR=="AAF16 MAIL GROUP",328,	
	ifelse (ETC$KEYWORDVAR=="ACV99 FILE",329,	
	ifelse (ETC$KEYWORDVAR=="ADL99 ITCHECK",330,	
	ifelse (ETC$KEYWORDVAR=="AAG01 PRINT ACCESS CONNECT",331,	
	ifelse (ETC$KEYWORDVAR=="ADZ01 REMOTE ACESS CONNECT",332,	
	ifelse (ETC$KEYWORDVAR=="AFH99 FIREWALL",333,	
	ifelse (ETC$KEYWORDVAR=="AGD99 COMPUTER",334,	
	ifelse (ETC$KEYWORDVAR=="AAW01 VM ACESS CONNECT",335,	
	ifelse (ETC$KEYWORDVAR=="AGG01 PC ACESS CONNECT",336,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAH13 LOGIN NOT WORK LAPTOP",337,	
	ifelse (ETC$KEYWORDVAR=="ABY99 VISIO",338,	
	ifelse (ETC$KEYWORDVAR=="ACD03 OUTLOOK ISSUE",339,	
	ifelse (ETC$KEYWORDVAR=="ADD99 FAX",340,	
	ifelse (ETC$KEYWORDVAR=="AHR99 PRODUCTION",341,	
	ifelse (ETC$KEYWORDVAR=="ADC02 COPIER DOWN NOTWORK",342,	
	ifelse (ETC$KEYWORDVAR=="AER99 DASHBOARD",343,	
	ifelse (ETC$KEYWORDVAR=="AAB22 SAP SETTING",344,	
	ifelse (ETC$KEYWORDVAR=="AAK04 INSTALL VPN",345,	
	ifelse (ETC$KEYWORDVAR=="ABT99 PDF",346,	
	ifelse (ETC$KEYWORDVAR=="AAA49 PASSWORD RESET",347,	
	ifelse (ETC$KEYWORDVAR=="AAB13 SAP ITEM",348,	
	ifelse (ETC$KEYWORDVAR=="AAF01 MAIL DELIVERY",349,	
	ifelse (ETC$KEYWORDVAR=="AAH40 LOGIN ISSUE FIS",350,	
	ifelse (ETC$KEYWORDVAR=="ACV02 FILE DOWN NOTWORK",351,	
	ifelse (ETC$KEYWORDVAR=="ADF99 INCIDENT",352,	
	ifelse (ETC$KEYWORDVAR=="ADH99 REQ",353,	
	ifelse (ETC$KEYWORDVAR=="AHA99 DATA",354,	
	ifelse (ETC$KEYWORDVAR=="AHY99 SCREEN",355,	
	ifelse (ETC$KEYWORDVAR=="OTHER",356,	
	ifelse (ETC$KEYWORDVAR=="AAE25 EMAIL ISSUE",357,	
	ifelse (ETC$KEYWORDVAR=="ADP99 USB",358,	
	ifelse (ETC$KEYWORDVAR=="AAE99 EMAIL",359,	
	ifelse (ETC$KEYWORDVAR=="AAK08 INSTALL VISIO",360,	
	ifelse (ETC$KEYWORDVAR=="AAL31 SETUP COMPUTER",361,	
	ifelse (ETC$KEYWORDVAR=="AAX03 EQUIPMENT ISSUE",362,	
	ifelse (ETC$KEYWORDVAR=="ABM03 BW ISSUE",363,	
	ifelse (ETC$KEYWORDVAR=="AAQ03 NETWORK DOWN NOTWORK",364,	
	ifelse (ETC$KEYWORDVAR=="ADA04 NEW SOFTWARE",365,	
	ifelse (ETC$KEYWORDVAR=="AAB16 SAP CHANGE",366,	
	ifelse (ETC$KEYWORDVAR=="ABS01 PROJECT ACESS CONNECT",367,	
	ifelse (ETC$KEYWORDVAR=="ADB99 PHONE",368,	
	ifelse (ETC$KEYWORDVAR=="AEW99 PART",369,	
	ifelse (ETC$KEYWORDVAR=="AFV99 MEET",370,	
	ifelse (ETC$KEYWORDVAR=="AIP99 IE",371,	
	ifelse (ETC$KEYWORDVAR=="AAA01 PASSWORD RESET EMAIL",372,	
	ifelse (ETC$KEYWORDVAR=="ADA05 NEW SECURITY",373,	
	ifelse (ETC$KEYWORDVAR=="AFD99 SALE",374,	
	ifelse (ETC$KEYWORDVAR=="AAB02 SAP DOWN NOTWORK",375,	
	ifelse (ETC$KEYWORDVAR=="AAE02 EMAIL ACCESS CONNECT",376,	
	ifelse (ETC$KEYWORDVAR=="AAG12 PRINT CHANGE",377,	
	ifelse (ETC$KEYWORDVAR=="AAH30 LOGIN ISSUE",378,	
	ifelse (ETC$KEYWORDVAR=="ABK03 REFUSOL ISSUE",379,	
	ifelse (ETC$KEYWORDVAR=="ABR01 SHAREPOINT ACESS CONNECT",380,	
	ifelse (ETC$KEYWORDVAR=="ACP99 SITE",381,	
	ifelse (ETC$KEYWORDVAR=="ACY02 APP ACESS CONNECT",382,	
	ifelse (ETC$KEYWORDVAR=="AEQ99 PTO",383,	
	ifelse (ETC$KEYWORDVAR=="AAB19 SAP SHIP",384,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="ABF99 SFDC",385,	
	ifelse (ETC$KEYWORDVAR=="ABR02 SHAREPOINT DOWN NOTWORK",386,	
	ifelse (ETC$KEYWORDVAR=="ACH99 DELL",387,	
	ifelse (ETC$KEYWORDVAR=="ACZ18 USERS",388,	
	ifelse (ETC$KEYWORDVAR=="AHQ99 PROCESS",389,	
	ifelse (ETC$KEYWORDVAR=="ACI01 CISCO ACESS CONNECT",390,	
	ifelse (ETC$KEYWORDVAR=="AAD05 VOICEMAIL ACCOUNT",391,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW SNC",392,	
	ifelse (ETC$KEYWORDVAR=="AID99 TRACK",393,	
	ifelse (ETC$KEYWORDVAR=="AAA37 PASSWORD RESET PC",394,	
	ifelse (ETC$KEYWORDVAR=="AAF21 MAIL TOOL",395,	
	ifelse (ETC$KEYWORDVAR=="AAK32 INSTALL ISSUE",396,	
	ifelse (ETC$KEYWORDVAR=="ABS99 PROJECT",397,	
	ifelse (ETC$KEYWORDVAR=="AEY99 ORDER",398,	
	ifelse (ETC$KEYWORDVAR=="AAG15 PRINT LABEL",399,	
	ifelse (ETC$KEYWORDVAR=="AFW99 CONFERENCE",400,	
	ifelse (ETC$KEYWORDVAR=="AGD02 COMPUTER DOWN NOTWORK",401,	
	ifelse (ETC$KEYWORDVAR=="AAB25 SAP OUTPUT",402,	
	ifelse (ETC$KEYWORDVAR=="AAB26 SAP DOCUMENT",403,	
	ifelse (ETC$KEYWORDVAR=="AAE12 EMAIL CHANGE",404,	
	ifelse (ETC$KEYWORDVAR=="AAH04 LOGIN NOT WORK DELL",405,	
	ifelse (ETC$KEYWORDVAR=="ACE99 EXCHANGE",406,	
	ifelse (ETC$KEYWORDVAR=="AGK99 VIRUS",407,	
	ifelse (ETC$KEYWORDVAR=="AAK20 INSTALL PHONE",408,	
	ifelse (ETC$KEYWORDVAR=="AAL14 SETUP CITRIX",409,	
	ifelse (ETC$KEYWORDVAR=="AGY99 DONGLE",410,	
	ifelse (ETC$KEYWORDVAR=="AHI99 GEAR",411,	
	ifelse (ETC$KEYWORDVAR=="ADA02 NEW GENERAL",412,	
	ifelse (ETC$KEYWORDVAR=="ADA99 NEW",413,	
	ifelse (ETC$KEYWORDVAR=="ADV02 PORTAL DOWN NOTWORK",414,	
	ifelse (ETC$KEYWORDVAR=="AEA01 LAN ACESS CONNECT",415,	
	ifelse (ETC$KEYWORDVAR=="AAS04 SWITCH COMPUTER",416,	
	ifelse (ETC$KEYWORDVAR=="AEB99 BOX",417,	
	ifelse (ETC$KEYWORDVAR=="ADI99 SNC",418,	
	ifelse (ETC$KEYWORDVAR=="AAB03 SAP LOGIN",419,	
	ifelse (ETC$KEYWORDVAR=="AAB36 SAP BOM",420,	
	ifelse (ETC$KEYWORDVAR=="ACI99 CISCO",421,	
	ifelse (ETC$KEYWORDVAR=="AGS99 CHANGE",422,	
	ifelse (ETC$KEYWORDVAR=="AIO99 OS",423,	
	ifelse (ETC$KEYWORDVAR=="AAF99 MAIL",424,	
	ifelse (ETC$KEYWORDVAR=="ABG99 FIS",425,	
	ifelse (ETC$KEYWORDVAR=="ABI01 PLM ACESS CONNECT",426,	
	ifelse (ETC$KEYWORDVAR=="ACY99 APP",427,	
	ifelse (ETC$KEYWORDVAR=="AEU01 PORT ACESS CONNECT",428,	
	ifelse (ETC$KEYWORDVAR=="AAU05 LINK ISSUE",429,	
	ifelse (ETC$KEYWORDVAR=="ADB12 PHONE ISSUE",430,	
	ifelse (ETC$KEYWORDVAR=="AEV99 MATERIAL",431,	
	ifelse (ETC$KEYWORDVAR=="AAA28 PASSWORD VPN",432,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAL15 SETUP DELL",433,	
	ifelse (ETC$KEYWORDVAR=="ABO01 FUSIONOPS ACESS CONNECT",434,	
	ifelse (ETC$KEYWORDVAR=="ACQ99 MCAFEE",435,	
	ifelse (ETC$KEYWORDVAR=="AIH99 TICKET",436,	
	ifelse (ETC$KEYWORDVAR=="AAA23 PASSWORD RESET USER",437,	
	ifelse (ETC$KEYWORDVAR=="AEH99 MONITOR",438,	
	ifelse (ETC$KEYWORDVAR=="AAB37 SAP BW",439,	
	ifelse (ETC$KEYWORDVAR=="AAZ99 JOB",440,	
	ifelse (ETC$KEYWORDVAR=="ACM99 SQL",441,	
	ifelse (ETC$KEYWORDVAR=="ABD99 PTOOL",442,	
	ifelse (ETC$KEYWORDVAR=="AGG03 PC ISSUE",443,	
	ifelse (ETC$KEYWORDVAR=="AAM99 CONFIG",444,	
	ifelse (ETC$KEYWORDVAR=="AAT02 NODE DOWN NOTWORK",445,	
	ifelse (ETC$KEYWORDVAR=="AAU02 LINK DOWN NOTWORK",446,	
	ifelse (ETC$KEYWORDVAR=="AAX99 EQUIPMENT",447,	
	ifelse (ETC$KEYWORDVAR=="ABM01 BW ACESS CONNECT",448,	
	ifelse (ETC$KEYWORDVAR=="AFU99 INFO",449,	
	ifelse (ETC$KEYWORDVAR=="ADA08 NEW ISSUE",450,	
	ifelse (ETC$KEYWORDVAR=="ADZ99 REMOTE",451,	
	ifelse (ETC$KEYWORDVAR=="AAE08 EMAIL PRINT",452,	
	ifelse (ETC$KEYWORDVAR=="AFJ99 CODE",453,	
	ifelse (ETC$KEYWORDVAR=="ACB03 OFFICE ISSUE",454,	
	ifelse (ETC$KEYWORDVAR=="ACZ15 USERS ACESS CONNECT",455,	
	ifelse (ETC$KEYWORDVAR=="AEZ99 WARRANTY",456,	
	ifelse (ETC$KEYWORDVAR=="AAM02 CONFIG OFFICE",457,	
	ifelse (ETC$KEYWORDVAR=="ACX13 SERVICE ISSUE",458,	
	ifelse (ETC$KEYWORDVAR=="AAB40 SAP ISSUE",459,	
	ifelse (ETC$KEYWORDVAR=="AAG99 PRINT",460,	
	ifelse (ETC$KEYWORDVAR=="AAV06 STATION ISSUE",461,	
	ifelse (ETC$KEYWORDVAR=="ACZ19 USER ACESS CONNECT",462,	
	ifelse (ETC$KEYWORDVAR=="AGH99 TIME",463,	
	ifelse (ETC$KEYWORDVAR=="ACB02 OFFICE DOWN NOTWORK",464,	
	ifelse (ETC$KEYWORDVAR=="AEN01 SPACE NEW",465,	
	ifelse (ETC$KEYWORDVAR=="AAE01 EMAIL DELIVERY",466,	
	ifelse (ETC$KEYWORDVAR=="ABG03 FIS ISSUE",467,	
	ifelse (ETC$KEYWORDVAR=="AIL99 MAT",468,	
	ifelse (ETC$KEYWORDVAR=="ADB03 PHONE DOWN NOTWORK",469,	
	ifelse (ETC$KEYWORDVAR=="ADE04 SCAN COMPUTER",470,	
	ifelse (ETC$KEYWORDVAR=="AAV01 STATION ACESS CONNECT",471,	
	ifelse (ETC$KEYWORDVAR=="AFC99 QUOTE",472,	
	ifelse (ETC$KEYWORDVAR=="ACP02 SITE DOWN NOTWORK",473,	
	ifelse (ETC$KEYWORDVAR=="AFT99 DOC",474,	
	ifelse (ETC$KEYWORDVAR=="ABB99 WINDOWS",475,	
	ifelse (ETC$KEYWORDVAR=="AAB99 SAP",476,	
	ifelse (ETC$KEYWORDVAR=="AIG99 WEB",477,	
	ifelse (ETC$KEYWORDVAR=="AHL99 LABEL",478,	
	ifelse (ETC$KEYWORDVAR=="ABN99 BOM",479,	
	ifelse (ETC$KEYWORDVAR=="ABS02 PROJECT DOWN NOTWORK",480,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AEG99 KEYBOARD",481,	
	ifelse (ETC$KEYWORDVAR=="AEU99 PORT",482,	
	ifelse (ETC$KEYWORDVAR=="AAQ99 NETWORK",483,	
	ifelse (ETC$KEYWORDVAR=="ADJ99 SQ",484,	
	ifelse (ETC$KEYWORDVAR=="AAM08 CONFIG ISSUE",485,	
	ifelse (ETC$KEYWORDVAR=="AGU99 HELP DESK",486,	
	ifelse (ETC$KEYWORDVAR=="AAF26 MAIL ISSUE",487,	
	ifelse (ETC$KEYWORDVAR=="ACH01 DELL ACESS CONNECT",488,	
	ifelse (ETC$KEYWORDVAR=="ACZ99 USER",489,	
	ifelse (ETC$KEYWORDVAR=="AAM07 CONFIG COMPUTER",490,	
	ifelse (ETC$KEYWORDVAR=="ABH03 EPDM ISSUE",491,	
	ifelse (ETC$KEYWORDVAR=="ABQ01 ENGINEER ACESS CONNECT",492,	
	ifelse (ETC$KEYWORDVAR=="ACX99 SERVICE",493,	
	ifelse (ETC$KEYWORDVAR=="AAE14 EMAIL NOTIFICATION",494,	
	ifelse (ETC$KEYWORDVAR=="ABK99 REFUSOL",495,	
	ifelse (ETC$KEYWORDVAR=="ADE10 SCAN ISSUE",496,	
	ifelse (ETC$KEYWORDVAR=="ADQ99 REPORT",497,	
	ifelse (ETC$KEYWORDVAR=="AFY99 CALENDAR",498,	
	ifelse (ETC$KEYWORDVAR=="ADB02 PHONE ACESS CONNECT",499,	
	ifelse (ETC$KEYWORDVAR=="ADV99 PORTAL",500,	
	ifelse (ETC$KEYWORDVAR=="AHH99 FORMAT",501,	
	ifelse (ETC$KEYWORDVAR=="AFX99 CALL",502,	
	ifelse (ETC$KEYWORDVAR=="AHC99 ASSET",503,	
	ifelse (ETC$KEYWORDVAR=="ACR99 SOLIDWORKS",504,	
	ifelse (ETC$KEYWORDVAR=="AAG17 PRINT ZEBRA",505,	
	ifelse (ETC$KEYWORDVAR=="AAC02 WEBEX ACESS",506,	
	ifelse (ETC$KEYWORDVAR=="AFA99 INVOICE",507,	
	ifelse (ETC$KEYWORDVAR=="AAL27 SETUP TS3",508,	
	ifelse (ETC$KEYWORDVAR=="ADE05 SCAN ADMIN",509,	
	ifelse (ETC$KEYWORDVAR=="ABE99 TOOL",510,	
	ifelse (ETC$KEYWORDVAR=="ADT02 INTERFACE DOWN NOTWORK",511,	
	ifelse (ETC$KEYWORDVAR=="ACC01 EXCEL ACESS CONNECT",512,	
	ifelse (ETC$KEYWORDVAR=="AGD03 COMPUTER ISSUE",513,	
	ifelse (ETC$KEYWORDVAR=="AAB11 SAP ORDER",514,	
	ifelse (ETC$KEYWORDVAR=="ACM03 SQL ISSUE",515,	
	ifelse (ETC$KEYWORDVAR=="ACY13 APP ISSUE",516,	
	ifelse (ETC$KEYWORDVAR=="ABM99 BW",517,	
	ifelse (ETC$KEYWORDVAR=="ABQ99 ENGINEER",518,	
	ifelse (ETC$KEYWORDVAR=="ACH03 DELL ISSUE",519,	
	ifelse (ETC$KEYWORDVAR=="ACA99 MS OFFICE",520,	
	ifelse (ETC$KEYWORDVAR=="AAE06 EMAIL SERVER",521,	
	ifelse (ETC$KEYWORDVAR=="AFE99 TAX",522,	
	ifelse (ETC$KEYWORDVAR=="ABN01 BOM ACESS CONNECT",523,	
	ifelse (ETC$KEYWORDVAR=="AAL02 SETUP VPN",524,	
	ifelse (ETC$KEYWORDVAR=="AIK99 LOG",525,	
	ifelse (ETC$KEYWORDVAR=="ADA01 NEW HARDWARE",526,	
	ifelse (ETC$KEYWORDVAR=="AAL24 SETUP NETWORK",527,	
	ifelse (ETC$KEYWORDVAR=="ACP03 SITE ISSUE",528,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AAG08 NEW PRINTER",529,	
	ifelse (ETC$KEYWORDVAR=="AAA19 PASSWORD RESET ACCOUNT",530,	
	ifelse (ETC$KEYWORDVAR=="AAO99 VPN",531,	
	ifelse (ETC$KEYWORDVAR=="ACO99 WEBSITE",532,	
	ifelse (ETC$KEYWORDVAR=="AAK12 INSTALL PROJECT",533,	
	ifelse (ETC$KEYWORDVAR=="ABJ99 TS3",534,	
	ifelse (ETC$KEYWORDVAR=="AII99 REFU",535,	
	ifelse (ETC$KEYWORDVAR=="ACK99 CAD",536,	
	ifelse (ETC$KEYWORDVAR=="ABF01 SFDC DOWN NOTWORK",537,	
	ifelse (ETC$KEYWORDVAR=="AFF99 BILL",538,	
	ifelse (ETC$KEYWORDVAR=="AEL99 RAM",539,	
	ifelse (ETC$KEYWORDVAR=="AAB23 SAP EXCEL",540,	
	ifelse (ETC$KEYWORDVAR=="AAC03 WEBEX DOWN NOTWORK",541,	
	ifelse (ETC$KEYWORDVAR=="AAP02 SERVER DOWN NOTWORK",542,	
	ifelse (ETC$KEYWORDVAR=="ADH01 DEBUG REQ",543,	
	ifelse (ETC$KEYWORDVAR=="ACI03 CISCO ISSUE",544,	
	ifelse (ETC$KEYWORDVAR=="AEX99 SHIP",545,	
	ifelse (ETC$KEYWORDVAR=="ABC99 WIN",546,	
	ifelse (ETC$KEYWORDVAR=="AEA02 LAN DOWN NOTWORK",547,	
	ifelse (ETC$KEYWORDVAR=="ABX99 ADP",548,	
	ifelse (ETC$KEYWORDVAR=="AEM99 MEMORY",549,	
	ifelse (ETC$KEYWORDVAR=="AAS99 SWITCH",550,	
	ifelse (ETC$KEYWORDVAR=="ACJ99 CITRIX",551,	
	ifelse (ETC$KEYWORDVAR=="ADC99 COPIER",552,	
	ifelse (ETC$KEYWORDVAR=="ADE99 SCAN",553,	
	ifelse (ETC$KEYWORDVAR=="AAE07 EMAIL ACCOUNT",554,	
	ifelse (ETC$KEYWORDVAR=="AGI99 HACK",555,	
	ifelse (ETC$KEYWORDVAR=="AAF02 MAIL ACCESS CONNECT",556,	
	ifelse (ETC$KEYWORDVAR=="AAB15 SAP REPORT",557,	
	ifelse (ETC$KEYWORDVAR=="AHJ99 INVERT",558,	
	ifelse (ETC$KEYWORDVAR=="AAH28 LOGIN CHANGE",559,	
	ifelse (ETC$KEYWORDVAR=="AAT99 NODE",560,	
	ifelse (ETC$KEYWORDVAR=="AEJ99 HEADSET",561,	
	ifelse (ETC$KEYWORDVAR=="ACU02 DRIVE DOWN NOTWORK",562,	
	ifelse (ETC$KEYWORDVAR=="AGA99 BATTERY",563,	
	ifelse (ETC$KEYWORDVAR=="ABL03 ASTEA ISSUE",564,	
	ifelse (ETC$KEYWORDVAR=="AHG99 FIRM",565,	
	ifelse (ETC$KEYWORDVAR=="AAP09 SERVER ACCOUNT",566,	
	ifelse (ETC$KEYWORDVAR=="AAH51 LOGIN ISSUE TOMCAT",567,	
	ifelse (ETC$KEYWORDVAR=="AAE05 EMAIL INSTALL",568,	
	ifelse (ETC$KEYWORDVAR=="AAL29 SETUP FOPS",569,	
	ifelse (ETC$KEYWORDVAR=="AAK24 INSTALL ENGINEER",570,	
	ifelse (ETC$KEYWORDVAR=="AAG16 PRINT INVOICE",571,	
	ifelse (ETC$KEYWORDVAR=="AHK99 ITEM",572,	
	ifelse (ETC$KEYWORDVAR=="ACW02 SYSTEM ACESS CONNECT",573,	
	ifelse (ETC$KEYWORDVAR=="ADO99 VOIP",574,	
	ifelse (ETC$KEYWORDVAR=="AAJ07 LOCK PHONE",575,	
	ifelse (ETC$KEYWORDVAR=="ACZ05 NEW HIRE HARDWARE",576,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_O3 <-	ifelse (ETC$KEYWORDVAR=="AHU99 RECRUIT",577,	
	ifelse (ETC$KEYWORDVAR=="ADS99 HARDWARE",578,	
	ifelse (ETC$KEYWORDVAR=="AET99 EXPENSE",579,	
	ifelse (ETC$KEYWORDVAR=="AHD99 ALERT",580,	
	ifelse (ETC$KEYWORDVAR=="AHB99 DEBUG",581,	
	ifelse (ETC$KEYWORDVAR=="ADG99 CRITICAL",582,	
	ifelse (ETC$KEYWORDVAR=="AAB04 SAP TRAINING",583,	
	ifelse (ETC$KEYWORDVAR=="ABL99 ASTEA",584,	
	ifelse (ETC$KEYWORDVAR=="ADT01 INTERFACE ACESS CONNECT",585,	
	ifelse (ETC$KEYWORDVAR=="AGZ99 DATABASE",586,	
	ifelse (ETC$KEYWORDVAR=="AIJ99 PAY",587,	
	ifelse (ETC$KEYWORDVAR=="AAH01 LOGIN NOT WORK BW",588,	
	ifelse (ETC$KEYWORDVAR=="AAH26 LOGIN LONG",589,	
	ifelse (ETC$KEYWORDVAR=="ADT03 INTERFACE ISSUE",590,	
	ifelse (ETC$KEYWORDVAR=="AAP12 SERVER ISSUE",591,	
	ifelse (ETC$KEYWORDVAR=="AAS01 SWITCH ACESS CONNECT",592,	
	ifelse (ETC$KEYWORDVAR=="AAQ11 NETWORK ACCOUNT",593,	
	ifelse (ETC$KEYWORDVAR=="AAV02 STATION DOWN NOTWORK",594,	paste(ETC$KEYWORDVAR_O3)))))))))))))))))))
	
ETC$KEYWORDVAR_O3 <- ifelse (ETC$KEYWORDVAR_O3 == "NA", 356, paste(ETC$KEYWORDVAR_O3))

ETC$KEYWORDVAR_R3 <- "NA"

		
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAA15 PASSWORD RESET PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK29 INSTALL EXPENS",1,	
	ifelse (ETC$KEYWORDVAR=="AAL25 SETUP INTERNATIONAL",1,	
	ifelse (ETC$KEYWORDVAR=="AAM05 CONFIG PROJECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAW99 VM",1,	
	ifelse (ETC$KEYWORDVAR=="ADD02 FAX DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AFP99 FTP",1,	
	ifelse (ETC$KEYWORDVAR=="AAA11 PASSWORD RESET COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="AAA13 PASSWORD RESET WINDOWS",1,	
	ifelse (ETC$KEYWORDVAR=="AAB35 SAP TS3",1,	
	ifelse (ETC$KEYWORDVAR=="AAC14 WEBEX TOOL",1,	
	ifelse (ETC$KEYWORDVAR=="AAL01 SETUP EPDM",1,	
	ifelse (ETC$KEYWORDVAR=="AAL10 SETUP ADOBE",1,	
	ifelse (ETC$KEYWORDVAR=="AAM01 CONFIG OUTLOOK",1,	
	ifelse (ETC$KEYWORDVAR=="ACF02 INTRANET DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACX07 SERVICE VIRUS HACK",1,	
	ifelse (ETC$KEYWORDVAR=="ACZ17 USERS ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ADD09 FAX ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AGW99 QUICKEN",1,	
	ifelse (ETC$KEYWORDVAR=="AAA47 PASSWORD RESET AUTO REPLY",1,	
	ifelse (ETC$KEYWORDVAR=="AAG13 PRINT CANCEL",1,	
	ifelse (ETC$KEYWORDVAR=="ACC02 EXCEL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADX03 WIRELESS ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AGL99 MALWARE",1,	
	ifelse (ETC$KEYWORDVAR=="AAE09 EMAIL SCAN",1,	
	ifelse (ETC$KEYWORDVAR=="AAE15 EMAIL AUTH",1,	
	ifelse (ETC$KEYWORDVAR=="AAK09 INSTALL GOOGLE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK31 INSTALL OS",1,	
	ifelse (ETC$KEYWORDVAR=="AAL08 SETUP FAX",1,	
	ifelse (ETC$KEYWORDVAR=="AAM04 CONFIG PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAO02 VPN DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ABJ01 TS3 ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAA16 PASSWORD PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK03 INSTALL FIS",1,	
	ifelse (ETC$KEYWORDVAR=="AAL11 SETUP ALTIUM",1,	
	ifelse (ETC$KEYWORDVAR=="AAL22 SETUP STATION",1,	
	ifelse (ETC$KEYWORDVAR=="ABM02 BW DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ABO99 FUSIONOPS",1,	
	ifelse (ETC$KEYWORDVAR=="ACO02 WEBSITE DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AGJ99 ANONYMOUS",1,	
	ifelse (ETC$KEYWORDVAR=="AAB39 SAP TICKET",1,	
	ifelse (ETC$KEYWORDVAR=="AAC10 WEBEX SETTING",1,	
	ifelse (ETC$KEYWORDVAR=="AAF05 MAIL INSTALL",1,	
	ifelse (ETC$KEYWORDVAR=="AAR02 ROUTER DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAS05 SWITCH ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ABE01 TOOL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ABZ02 SALESFORCE DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADY01 WIFI ACESS CONNECT",1,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAA36 PASSWORD PCARD",1,	
	ifelse (ETC$KEYWORDVAR=="AAK10 INSTALL OUTLOOK",1,	
	ifelse (ETC$KEYWORDVAR=="AAK13 INSTALL SCAN",1,	
	ifelse (ETC$KEYWORDVAR=="AAR04 ROUTER ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAV03 STATION ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="ABP03 ALTIUM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AHV99 RESET",1,	
	ifelse (ETC$KEYWORDVAR=="AAE19 EMAIL JUNK",1,	
	ifelse (ETC$KEYWORDVAR=="AAO03 VPN ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ADN99 CPU",1,	
	ifelse (ETC$KEYWORDVAR=="ADW01 DOMAIN ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AFR99 TONER",1,	
	ifelse (ETC$KEYWORDVAR=="AAC01 WEBEX DISABLE",1,	
	ifelse (ETC$KEYWORDVAR=="AAF10 MAIL PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAL17 SETUP PDF",1,	
	ifelse (ETC$KEYWORDVAR=="ACF99 INTRANET",1,	
	ifelse (ETC$KEYWORDVAR=="ACV01 FILE ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AGH01 TIME ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAH48 LOGIN ISSUE REMOTE",1,	
	ifelse (ETC$KEYWORDVAR=="AAK05 INSTALL CHINESE",1,	
	ifelse (ETC$KEYWORDVAR=="ACB01 OFFICE ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AHS99 PRODUCT",1,	
	ifelse (ETC$KEYWORDVAR=="AAG04 PRINT LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAL16 SETUP DRIVE",1,	
	ifelse (ETC$KEYWORDVAR=="AFI99 BARCODE",1,	
	ifelse (ETC$KEYWORDVAR=="AAA10 PASSWORD WEBEX",1,	
	ifelse (ETC$KEYWORDVAR=="AAL04 SETUP WINDOWS",1,	
	ifelse (ETC$KEYWORDVAR=="AAA02 PASSWORD EMAIL",1,	
	ifelse (ETC$KEYWORDVAR=="AAA48 PASSWORD RESET AUTO",1,	
	ifelse (ETC$KEYWORDVAR=="AAP05 SERVER RESTORE",1,	
	ifelse (ETC$KEYWORDVAR=="AAP08 SERVER ROLLOUT",1,	
	ifelse (ETC$KEYWORDVAR=="AAW02 VM DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAF20 MAIL OUTLOOK",1,	
	ifelse (ETC$KEYWORDVAR=="AGG02 PC DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAG07 PRINT INK",1,	
	ifelse (ETC$KEYWORDVAR=="AAK30 INSTALL IE",1,	
	ifelse (ETC$KEYWORDVAR=="ABL02 ASTEA DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACC99 EXCEL",1,	
	ifelse (ETC$KEYWORDVAR=="ACT02 FOLDER DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AGF02 DESKTOP DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAB24 SAP PART",1,	
	ifelse (ETC$KEYWORDVAR=="AAF25 MAIL FULL",1,	
	ifelse (ETC$KEYWORDVAR=="AAD01 VOICEMAIL DISABLE",1,	
	ifelse (ETC$KEYWORDVAR=="AAG09 FIX PRINTER",1,	
	ifelse (ETC$KEYWORDVAR=="ABP99 ALTIUM",1,	
	ifelse (ETC$KEYWORDVAR=="ACD02 OUTLOOK DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACM01 SQL ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ADB04 FIX PHONE",1,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAF12 MAIL CHANGE",1,	
	ifelse (ETC$KEYWORDVAR=="AGC99 MACHINE",1,	
	ifelse (ETC$KEYWORDVAR=="AIQ99 TRAINING",1,	
	ifelse (ETC$KEYWORDVAR=="AAL09 SETUP ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="ACZ20 USER DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AIM99 CART",1,	
	ifelse (ETC$KEYWORDVAR=="AAQ14 NETWORK ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAE03 EMAIL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAJ04 LOCK COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="ACF01 INTRANET ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ADW02 DOMAIN DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAE10 EMAIL PHONE",1,	
	ifelse (ETC$KEYWORDVAR=="AAE16 EMAIL GROUP",1,	
	ifelse (ETC$KEYWORDVAR=="AIF99 UPS",1,	
	ifelse (ETC$KEYWORDVAR=="AAK33 INSTALL SOFTWARE",1,	
	ifelse (ETC$KEYWORDVAR=="AAL20 SETUP WIRELESS",1,	
	ifelse (ETC$KEYWORDVAR=="AAT03 NODE REBOOT",1,	
	ifelse (ETC$KEYWORDVAR=="ABJ03 TS3 ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ABL01 ASTEA ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ACW13 SYSTEM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AHP99 PACK",1,	
	ifelse (ETC$KEYWORDVAR=="AAC17 WEBEX CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAG02 PRINT DOWN NOTWORK PROD",1,	
	ifelse (ETC$KEYWORDVAR=="AAH31 LOGIN ISSUE BW",1,	
	ifelse (ETC$KEYWORDVAR=="AAL26 SETUP TECHNICIAN",1,	
	ifelse (ETC$KEYWORDVAR=="ABI03 PLM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="ACP01 SITE ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ADX99 WIRELESS",1,	
	ifelse (ETC$KEYWORDVAR=="AEI99 MOUSE",1,	
	ifelse (ETC$KEYWORDVAR=="AGF03 DESKTOP ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAE04 EMAIL LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAK23 INSTALL DRIVE",1,	
	ifelse (ETC$KEYWORDVAR=="AAL28 SETUP SFDC",1,	
	ifelse (ETC$KEYWORDVAR=="ABI99 PLM",1,	
	ifelse (ETC$KEYWORDVAR=="AAB07 SAP NEW ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="AAG11 PRINT AUTO",1,	
	ifelse (ETC$KEYWORDVAR=="AAH24 LOGIN NOT WORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADB09 PHONE DELIVERY",1,	
	ifelse (ETC$KEYWORDVAR=="AAL05 SETUP OUTLOOK",1,	
	ifelse (ETC$KEYWORDVAR=="ABN03 BOM ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AFM99 LIST",1,	
	ifelse (ETC$KEYWORDVAR=="AAD99 VOICEMAIL",1,	
	ifelse (ETC$KEYWORDVAR=="AAH03 LOGIN NOT WORK COMPUTER",1,	
	ifelse (ETC$KEYWORDVAR=="AAZ01 JOB STEP1",1,	
	ifelse (ETC$KEYWORDVAR=="ACX11 SERVICE ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAH99 LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAL30 SETUP SERVRER",1,	
	ifelse (ETC$KEYWORDVAR=="AAP01 SERVER ACESS CONNECT",1,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="ADT99 INTERFACE",1,	
	ifelse (ETC$KEYWORDVAR=="AHN99 MESSAGE",1,	
	ifelse (ETC$KEYWORDVAR=="AAB06 SAP NEW REQUEST",1,	
	ifelse (ETC$KEYWORDVAR=="AAB21 SAP GROUP",1,	
	ifelse (ETC$KEYWORDVAR=="AAB32 SAP PORTAL",1,	
	ifelse (ETC$KEYWORDVAR=="AAF03 MAIL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AAL23 SETUP TOOL",1,	
	ifelse (ETC$KEYWORDVAR=="ABU99 ADOBE",1,	
	ifelse (ETC$KEYWORDVAR=="ACX10 SERVICE ACCOUNT",1,	
	ifelse (ETC$KEYWORDVAR=="ADV03 PORTAL ISSUE",1,	
	ifelse (ETC$KEYWORDVAR=="AAJ99 LOCK",1,	
	ifelse (ETC$KEYWORDVAR=="AAK15 INSTALL ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAK19 INSTALL ALTIUM",1,	
	ifelse (ETC$KEYWORDVAR=="ABD01 PTOOL DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AIE99 UPLOAD",1,	
	ifelse (ETC$KEYWORDVAR=="AAC05 WEBEX INSTALL",1,	
	ifelse (ETC$KEYWORDVAR=="ABW99 ATS",1,	
	ifelse (ETC$KEYWORDVAR=="ACE02 EXCHANGE DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACG99 EXTRANET",1,	
	ifelse (ETC$KEYWORDVAR=="ADB08 PHONE CHANGE",1,	
	ifelse (ETC$KEYWORDVAR=="AFQ99 EFT",1,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP INSTALL",1,	
	ifelse (ETC$KEYWORDVAR=="AAC04 WEBEX LOGIN",1,	
	ifelse (ETC$KEYWORDVAR=="AAI02 BLOCK SITE",1,	
	ifelse (ETC$KEYWORDVAR=="AAP10 SERVER ADMIN",1,	
	ifelse (ETC$KEYWORDVAR=="ABG01 FIS ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="ADV01 PORTAL ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AAZ02 JOB DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ACO01 WEBSITE ACESS CONNECT",1,	
	ifelse (ETC$KEYWORDVAR=="AHM99 LICENSE",1,	
	ifelse (ETC$KEYWORDVAR=="AAE26 EMAIL REQUEST",1,	
	ifelse (ETC$KEYWORDVAR=="AAN02 INTERNET DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADE02 SCAN DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="ADR99 SOFTWARE",1,	
	ifelse (ETC$KEYWORDVAR=="ADX02 WIRELESS DOWN NOTWORK",1,	
	ifelse (ETC$KEYWORDVAR=="AFS99 INK",1,	
	ifelse (ETC$KEYWORDVAR=="AAK26 INSTALL PDF",2,	
	ifelse (ETC$KEYWORDVAR=="AAP04 SERVER BACKUP",2,	
	ifelse (ETC$KEYWORDVAR=="ABF02 SFDC ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACY11 APP ADMIN",2,	
	ifelse (ETC$KEYWORDVAR=="ADM99 FLASH",2,	
	ifelse (ETC$KEYWORDVAR=="AAC99 WEBEX",2,	
	ifelse (ETC$KEYWORDVAR=="AAK02 INSTALL EPDM",2,	
	ifelse (ETC$KEYWORDVAR=="AGX99 ENERGY",2,	
	ifelse (ETC$KEYWORDVAR=="AAU01 LINK ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="ACW10 SYSTEM ACCOUNT",2,	
	ifelse (ETC$KEYWORDVAR=="AEC99 IPAD IPHONE",2,	
	ifelse (ETC$KEYWORDVAR=="AFN99 ROLE",2,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAB33 SAP REFUSOL",2,	
	ifelse (ETC$KEYWORDVAR=="AAE20 EMAIL OUTLOOK",2,	
	ifelse (ETC$KEYWORDVAR=="ABH99 EPDM",2,	
	ifelse (ETC$KEYWORDVAR=="AGF99 DESKTOP",2,	
	ifelse (ETC$KEYWORDVAR=="AAA39 PASSWORD RESET TS3",2,	
	ifelse (ETC$KEYWORDVAR=="AAC06 WEBEX ACCOUNT",2,	
	ifelse (ETC$KEYWORDVAR=="ADB01 PHONE COMPANY",2,	
	ifelse (ETC$KEYWORDVAR=="AEU03 PORT ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="AAA07 PASSWORD RESET SAP",2,	
	ifelse (ETC$KEYWORDVAR=="ABE02 TOOL ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACK03 CAD ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACZ12 NEW HIRE ACCOUNT",2,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW PHONE",2,	
	ifelse (ETC$KEYWORDVAR=="AIN99 CAR",2,	
	ifelse (ETC$KEYWORDVAR=="AAA99 PASSWORD",2,	
	ifelse (ETC$KEYWORDVAR=="AAB14 SAP MATERIAL",2,	
	ifelse (ETC$KEYWORDVAR=="AAN01 INTERNET ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="ABH02 EPDM DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR=="ACW99 SYSTEM",2,	
	ifelse (ETC$KEYWORDVAR=="AAE21 EMAIL TOOL",2,	
	ifelse (ETC$KEYWORDVAR=="AAF04 MAIL LOGIN",2,	
	ifelse (ETC$KEYWORDVAR=="AAG20 PRINT ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="AAK99 INSTALL",2,	
	ifelse (ETC$KEYWORDVAR=="ACU99 DRIVE",2,	
	ifelse (ETC$KEYWORDVAR=="ACW01 SYSTEM UNATHOR ACCESS",2,	
	ifelse (ETC$KEYWORDVAR=="AFO99 BACKUP",2,	
	ifelse (ETC$KEYWORDVAR=="AHO99 OUTPUT",2,	
	ifelse (ETC$KEYWORDVAR=="AAH12 LOGIN NOT WORK INTERNET",2,	
	ifelse (ETC$KEYWORDVAR=="AAH23 LOGIN NOT WORK VPN",2,	
	ifelse (ETC$KEYWORDVAR=="AAL12 SETUP PHONE",2,	
	ifelse (ETC$KEYWORDVAR=="ABZ99 SALESFORCE",2,	
	ifelse (ETC$KEYWORDVAR=="AAI99 BLOCK",2,	
	ifelse (ETC$KEYWORDVAR=="AAK16 INSTALL CAD",2,	
	ifelse (ETC$KEYWORDVAR=="AAP07 SERVER FILE",2,	
	ifelse (ETC$KEYWORDVAR=="AAX01 EQUIPMENT FOLLOWUP",2,	
	ifelse (ETC$KEYWORDVAR=="ACX03 SERVICE DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR=="ADB11 PHONE DIRECTORY",2,	
	ifelse (ETC$KEYWORDVAR=="AFL99 CASE",2,	
	ifelse (ETC$KEYWORDVAR=="AGD01 COMPUTER ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AAN99 INTERNET",2,	
	ifelse (ETC$KEYWORDVAR=="ABD02 PTOOL ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ADG01 CRITICAL ALERT",2,	
	ifelse (ETC$KEYWORDVAR=="AAH33 LOGIN ISSUE COMPUTER",2,	
	ifelse (ETC$KEYWORDVAR=="AIR99 TEST",2,	
	ifelse (ETC$KEYWORDVAR=="AAB09 SAP PRINT",2,	
	ifelse (ETC$KEYWORDVAR=="AAB10 SAP CODE",2,	
	ifelse (ETC$KEYWORDVAR=="AAK01 INSTALL PLM",2,	
	ifelse (ETC$KEYWORDVAR=="AAK11 INSTALL OFFICE",2,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAQ01 NETWORK WIRELESS",2,	
	ifelse (ETC$KEYWORDVAR=="ACD99 OUTLOOK",2,	
	ifelse (ETC$KEYWORDVAR=="ACZ11 NEW HIRE CODE",2,	
	ifelse (ETC$KEYWORDVAR=="ACD01 OUTLOOK ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="ACZ14 NEW HIRE",2,	
	ifelse (ETC$KEYWORDVAR=="ADX01 WIRELESS ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AEN99 SPACE",2,	
	ifelse (ETC$KEYWORDVAR=="AFG99 DELIVER",2,	
	ifelse (ETC$KEYWORDVAR=="AAI01 BLOCK VENDER",2,	
	ifelse (ETC$KEYWORDVAR=="AAQ10 NETWORK COMPUTER",2,	
	ifelse (ETC$KEYWORDVAR=="AAY99 INVENTORY",2,	
	ifelse (ETC$KEYWORDVAR=="ABK01 REFUSOL ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="ABS03 PROJECT ISSUE",2,	
	ifelse (ETC$KEYWORDVAR=="ACH02 DELL DOWN NOTWORK",2,	
	ifelse (ETC$KEYWORDVAR=="ACT99 FOLDER",2,	
	ifelse (ETC$KEYWORDVAR=="AGQ99 ADMIN",2,	
	ifelse (ETC$KEYWORDVAR=="AAB12 SAP INVENTORY",2,	
	ifelse (ETC$KEYWORDVAR=="ABH01 EPDM ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AAO01 VPN ACESS CONNECT",2,	
	ifelse (ETC$KEYWORDVAR=="AAP06 SERVER VIRUS HACK",2,	
	ifelse (ETC$KEYWORDVAR=="ABV99 GOOGLE",2,	
	ifelse (ETC$KEYWORDVAR=="AAB17 SAP AUTH",3,	
	ifelse (ETC$KEYWORDVAR=="AAG05 PRINT INSTALL",3,	
	ifelse (ETC$KEYWORDVAR=="ABQ03 ENGINEER ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAJ10 LOCK ACCOUNT",3,	
	ifelse (ETC$KEYWORDVAR=="AAN03 INTERNET ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AEO99 VOLUME",3,	
	ifelse (ETC$KEYWORDVAR=="AGB99 POWER",3,	
	ifelse (ETC$KEYWORDVAR=="AHT99 RECORD",3,	
	ifelse (ETC$KEYWORDVAR=="AAA08 PASSWORD SAP",3,	
	ifelse (ETC$KEYWORDVAR=="AAC12 WEBEX MEETING",3,	
	ifelse (ETC$KEYWORDVAR=="AAU99 LINK",3,	
	ifelse (ETC$KEYWORDVAR=="AHF99 DIRECTORY",3,	
	ifelse (ETC$KEYWORDVAR=="AAB08 SAP ACCOUNT",3,	
	ifelse (ETC$KEYWORDVAR=="AAC15 WEBEX ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="ABZ01 SALESFORCE ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ACM02 SQL DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ACX02 SERVICE ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ACC03 EXCEL ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAK22 INSTALL DELL",3,	
	ifelse (ETC$KEYWORDVAR=="AAP99 SERVER",3,	
	ifelse (ETC$KEYWORDVAR=="AAQ02 NETWORK ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ACU03 DRIVE ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAK28 INSTALL SQL",3,	
	ifelse (ETC$KEYWORDVAR=="AAV99 STATION",3,	
	ifelse (ETC$KEYWORDVAR=="AEA99 LAN",3,	
	ifelse (ETC$KEYWORDVAR=="AEU02 PORT DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="AIA99 SPYWARE",3,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="ABR03 SHAREPOINT ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="ACY10 APP ACCOUNT",3,	
	ifelse (ETC$KEYWORDVAR=="ACZ21 USER ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AES99 WG",3,	
	ifelse (ETC$KEYWORDVAR=="AGH03 TIME ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AAS02 SWITCH DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ACW03 SYSTEM DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ADF01 INCIDENT ATTACK",3,	
	ifelse (ETC$KEYWORDVAR=="AAA30 PASSWORD ASTEA",3,	
	ifelse (ETC$KEYWORDVAR=="AAL99 SETUP",3,	
	ifelse (ETC$KEYWORDVAR=="AAV05 STATION COMPUTER",3,	
	ifelse (ETC$KEYWORDVAR=="ACU01 DRIVE ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AAB01 SAP ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="ABR99 SHAREPOINT",3,	
	ifelse (ETC$KEYWORDVAR=="ACB99 OFFICE",3,	
	ifelse (ETC$KEYWORDVAR=="AGO99 DOWN",3,	
	ifelse (ETC$KEYWORDVAR=="AAB41 SAP REQUEST",3,	
	ifelse (ETC$KEYWORDVAR=="AAG03 PRINT DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ABG02 FIS DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="AFB99 PURCHASE",3,	
	ifelse (ETC$KEYWORDVAR=="ACT01 FOLDER ACESS CONNECT",3,	
	ifelse (ETC$KEYWORDVAR=="AAK07 INSTALL WINDOWS",3,	
	ifelse (ETC$KEYWORDVAR=="ACK02 CAD DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="ADA07 NEW COMPUTER",3,	
	ifelse (ETC$KEYWORDVAR=="AAM06 CONFIG FIS",3,	
	ifelse (ETC$KEYWORDVAR=="AAW05 VM COMPUTER",3,	
	ifelse (ETC$KEYWORDVAR=="ACT03 FOLDER ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AEF99 MOVE",3,	
	ifelse (ETC$KEYWORDVAR=="AGP99 ACCESS",3,	
	ifelse (ETC$KEYWORDVAR=="AAC08 WEBEX CHANGE",3,	
	ifelse (ETC$KEYWORDVAR=="ACY03 APP DOWN NOTWORK",3,	
	ifelse (ETC$KEYWORDVAR=="AGG99 PC",3,	
	ifelse (ETC$KEYWORDVAR=="AGT99 ISSUE",3,	
	ifelse (ETC$KEYWORDVAR=="AIC99 TRANSACTION",3,	
	ifelse (ETC$KEYWORDVAR=="AAF07 MAIL ACCOUNT",4,	
	ifelse (ETC$KEYWORDVAR=="ADW99 DOMAIN",4,	
	ifelse (ETC$KEYWORDVAR=="AFK99 GROUP",4,	
	ifelse (ETC$KEYWORDVAR=="AFZ99 ADDRESS",4,	
	ifelse (ETC$KEYWORDVAR=="ACZ16 USERS DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="AAF16 MAIL GROUP",4,	
	ifelse (ETC$KEYWORDVAR=="ACV99 FILE",4,	
	ifelse (ETC$KEYWORDVAR=="ADL99 ITCHECK",4,	
	ifelse (ETC$KEYWORDVAR=="AAG01 PRINT ACCESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="ADZ01 REMOTE ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AFH99 FIREWALL",4,	
	ifelse (ETC$KEYWORDVAR=="AGD99 COMPUTER",4,	
	ifelse (ETC$KEYWORDVAR=="AAW01 VM ACESS CONNECT",4,	
	ifelse (ETC$KEYWORDVAR=="AGG01 PC ACESS CONNECT",4,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAH13 LOGIN NOT WORK LAPTOP",4,	
	ifelse (ETC$KEYWORDVAR=="ABY99 VISIO",4,	
	ifelse (ETC$KEYWORDVAR=="ACD03 OUTLOOK ISSUE",4,	
	ifelse (ETC$KEYWORDVAR=="ADD99 FAX",4,	
	ifelse (ETC$KEYWORDVAR=="AHR99 PRODUCTION",4,	
	ifelse (ETC$KEYWORDVAR=="ADC02 COPIER DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="AER99 DASHBOARD",4,	
	ifelse (ETC$KEYWORDVAR=="AAB22 SAP SETTING",4,	
	ifelse (ETC$KEYWORDVAR=="AAK04 INSTALL VPN",4,	
	ifelse (ETC$KEYWORDVAR=="ABT99 PDF",4,	
	ifelse (ETC$KEYWORDVAR=="AAA49 PASSWORD RESET",4,	
	ifelse (ETC$KEYWORDVAR=="AAB13 SAP ITEM",4,	
	ifelse (ETC$KEYWORDVAR=="AAF01 MAIL DELIVERY",4,	
	ifelse (ETC$KEYWORDVAR=="AAH40 LOGIN ISSUE FIS",4,	
	ifelse (ETC$KEYWORDVAR=="ACV02 FILE DOWN NOTWORK",4,	
	ifelse (ETC$KEYWORDVAR=="ADF99 INCIDENT",4,	
	ifelse (ETC$KEYWORDVAR=="ADH99 REQ",4,	
	ifelse (ETC$KEYWORDVAR=="AHA99 DATA",4,	
	ifelse (ETC$KEYWORDVAR=="AHY99 SCREEN",4,	
	ifelse (ETC$KEYWORDVAR=="OTHER",4,	
	ifelse (ETC$KEYWORDVAR=="AAE25 EMAIL ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="ADP99 USB",5,	
	ifelse (ETC$KEYWORDVAR=="AAE99 EMAIL",5,	
	ifelse (ETC$KEYWORDVAR=="AAK08 INSTALL VISIO",5,	
	ifelse (ETC$KEYWORDVAR=="AAL31 SETUP COMPUTER",5,	
	ifelse (ETC$KEYWORDVAR=="AAX03 EQUIPMENT ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="ABM03 BW ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="AAQ03 NETWORK DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="ADA04 NEW SOFTWARE",5,	
	ifelse (ETC$KEYWORDVAR=="AAB16 SAP CHANGE",5,	
	ifelse (ETC$KEYWORDVAR=="ABS01 PROJECT ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="ADB99 PHONE",5,	
	ifelse (ETC$KEYWORDVAR=="AEW99 PART",5,	
	ifelse (ETC$KEYWORDVAR=="AFV99 MEET",5,	
	ifelse (ETC$KEYWORDVAR=="AIP99 IE",5,	
	ifelse (ETC$KEYWORDVAR=="AAA01 PASSWORD RESET EMAIL",5,	
	ifelse (ETC$KEYWORDVAR=="ADA05 NEW SECURITY",5,	
	ifelse (ETC$KEYWORDVAR=="AFD99 SALE",5,	
	ifelse (ETC$KEYWORDVAR=="AAB02 SAP DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="AAE02 EMAIL ACCESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AAG12 PRINT CHANGE",5,	
	ifelse (ETC$KEYWORDVAR=="AAH30 LOGIN ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="ABK03 REFUSOL ISSUE",5,	
	ifelse (ETC$KEYWORDVAR=="ABR01 SHAREPOINT ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="ACP99 SITE",5,	
	ifelse (ETC$KEYWORDVAR=="ACY02 APP ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AEQ99 PTO",5,	
	ifelse (ETC$KEYWORDVAR=="AAB19 SAP SHIP",5,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="ABF99 SFDC",5,	
	ifelse (ETC$KEYWORDVAR=="ABR02 SHAREPOINT DOWN NOTWORK",5,	
	ifelse (ETC$KEYWORDVAR=="ACH99 DELL",5,	
	ifelse (ETC$KEYWORDVAR=="ACZ18 USERS",5,	
	ifelse (ETC$KEYWORDVAR=="AHQ99 PROCESS",5,	
	ifelse (ETC$KEYWORDVAR=="ACI01 CISCO ACESS CONNECT",5,	
	ifelse (ETC$KEYWORDVAR=="AAD05 VOICEMAIL ACCOUNT",5,	
	ifelse (ETC$KEYWORDVAR=="ADA06 NEW SNC",5,	
	ifelse (ETC$KEYWORDVAR=="AID99 TRACK",5,	
	ifelse (ETC$KEYWORDVAR=="AAA37 PASSWORD RESET PC",6,	
	ifelse (ETC$KEYWORDVAR=="AAF21 MAIL TOOL",6,	
	ifelse (ETC$KEYWORDVAR=="AAK32 INSTALL ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="ABS99 PROJECT",6,	
	ifelse (ETC$KEYWORDVAR=="AEY99 ORDER",6,	
	ifelse (ETC$KEYWORDVAR=="AAG15 PRINT LABEL",6,	
	ifelse (ETC$KEYWORDVAR=="AFW99 CONFERENCE",6,	
	ifelse (ETC$KEYWORDVAR=="AGD02 COMPUTER DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="AAB25 SAP OUTPUT",6,	
	ifelse (ETC$KEYWORDVAR=="AAB26 SAP DOCUMENT",6,	
	ifelse (ETC$KEYWORDVAR=="AAE12 EMAIL CHANGE",6,	
	ifelse (ETC$KEYWORDVAR=="AAH04 LOGIN NOT WORK DELL",6,	
	ifelse (ETC$KEYWORDVAR=="ACE99 EXCHANGE",6,	
	ifelse (ETC$KEYWORDVAR=="AGK99 VIRUS",6,	
	ifelse (ETC$KEYWORDVAR=="AAK20 INSTALL PHONE",6,	
	ifelse (ETC$KEYWORDVAR=="AAL14 SETUP CITRIX",6,	
	ifelse (ETC$KEYWORDVAR=="AGY99 DONGLE",6,	
	ifelse (ETC$KEYWORDVAR=="AHI99 GEAR",6,	
	ifelse (ETC$KEYWORDVAR=="ADA02 NEW GENERAL",6,	
	ifelse (ETC$KEYWORDVAR=="ADA99 NEW",6,	
	ifelse (ETC$KEYWORDVAR=="ADV02 PORTAL DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="AEA01 LAN ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AAS04 SWITCH COMPUTER",6,	
	ifelse (ETC$KEYWORDVAR=="AEB99 BOX",6,	
	ifelse (ETC$KEYWORDVAR=="ADI99 SNC",6,	
	ifelse (ETC$KEYWORDVAR=="AAB03 SAP LOGIN",6,	
	ifelse (ETC$KEYWORDVAR=="AAB36 SAP BOM",6,	
	ifelse (ETC$KEYWORDVAR=="ACI99 CISCO",6,	
	ifelse (ETC$KEYWORDVAR=="AGS99 CHANGE",6,	
	ifelse (ETC$KEYWORDVAR=="AIO99 OS",6,	
	ifelse (ETC$KEYWORDVAR=="AAF99 MAIL",6,	
	ifelse (ETC$KEYWORDVAR=="ABG99 FIS",6,	
	ifelse (ETC$KEYWORDVAR=="ABI01 PLM ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="ACY99 APP",6,	
	ifelse (ETC$KEYWORDVAR=="AEU01 PORT ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AAU05 LINK ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="ADB12 PHONE ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="AEV99 MATERIAL",6,	
	ifelse (ETC$KEYWORDVAR=="AAA28 PASSWORD VPN",6,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAL15 SETUP DELL",6,	
	ifelse (ETC$KEYWORDVAR=="ABO01 FUSIONOPS ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="ACQ99 MCAFEE",6,	
	ifelse (ETC$KEYWORDVAR=="AIH99 TICKET",6,	
	ifelse (ETC$KEYWORDVAR=="AAA23 PASSWORD RESET USER",6,	
	ifelse (ETC$KEYWORDVAR=="AEH99 MONITOR",6,	
	ifelse (ETC$KEYWORDVAR=="AAB37 SAP BW",6,	
	ifelse (ETC$KEYWORDVAR=="AAZ99 JOB",6,	
	ifelse (ETC$KEYWORDVAR=="ACM99 SQL",6,	
	ifelse (ETC$KEYWORDVAR=="ABD99 PTOOL",6,	
	ifelse (ETC$KEYWORDVAR=="AGG03 PC ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="AAM99 CONFIG",6,	
	ifelse (ETC$KEYWORDVAR=="AAT02 NODE DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="AAU02 LINK DOWN NOTWORK",6,	
	ifelse (ETC$KEYWORDVAR=="AAX99 EQUIPMENT",6,	
	ifelse (ETC$KEYWORDVAR=="ABM01 BW ACESS CONNECT",6,	
	ifelse (ETC$KEYWORDVAR=="AFU99 INFO",6,	
	ifelse (ETC$KEYWORDVAR=="ADA08 NEW ISSUE",6,	
	ifelse (ETC$KEYWORDVAR=="ADZ99 REMOTE",6,	
	ifelse (ETC$KEYWORDVAR=="AAE08 EMAIL PRINT",6,	
	ifelse (ETC$KEYWORDVAR=="AFJ99 CODE",6,	
	ifelse (ETC$KEYWORDVAR=="ACB03 OFFICE ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ACZ15 USERS ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AEZ99 WARRANTY",7,	
	ifelse (ETC$KEYWORDVAR=="AAM02 CONFIG OFFICE",7,	
	ifelse (ETC$KEYWORDVAR=="ACX13 SERVICE ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AAB40 SAP ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AAG99 PRINT",7,	
	ifelse (ETC$KEYWORDVAR=="AAV06 STATION ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ACZ19 USER ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AGH99 TIME",7,	
	ifelse (ETC$KEYWORDVAR=="ACB02 OFFICE DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AEN01 SPACE NEW",7,	
	ifelse (ETC$KEYWORDVAR=="AAE01 EMAIL DELIVERY",7,	
	ifelse (ETC$KEYWORDVAR=="ABG03 FIS ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AIL99 MAT",7,	
	ifelse (ETC$KEYWORDVAR=="ADB03 PHONE DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="ADE04 SCAN COMPUTER",7,	
	ifelse (ETC$KEYWORDVAR=="AAV01 STATION ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="AFC99 QUOTE",7,	
	ifelse (ETC$KEYWORDVAR=="ACP02 SITE DOWN NOTWORK",7,	
	ifelse (ETC$KEYWORDVAR=="AFT99 DOC",7,	
	ifelse (ETC$KEYWORDVAR=="ABB99 WINDOWS",7,	
	ifelse (ETC$KEYWORDVAR=="AAB99 SAP",7,	
	ifelse (ETC$KEYWORDVAR=="AIG99 WEB",7,	
	ifelse (ETC$KEYWORDVAR=="AHL99 LABEL",7,	
	ifelse (ETC$KEYWORDVAR=="ABN99 BOM",7,	
	ifelse (ETC$KEYWORDVAR=="ABS02 PROJECT DOWN NOTWORK",7,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AEG99 KEYBOARD",7,	
	ifelse (ETC$KEYWORDVAR=="AEU99 PORT",7,	
	ifelse (ETC$KEYWORDVAR=="AAQ99 NETWORK",7,	
	ifelse (ETC$KEYWORDVAR=="ADJ99 SQ",7,	
	ifelse (ETC$KEYWORDVAR=="AAM08 CONFIG ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="AGU99 HELP DESK",7,	
	ifelse (ETC$KEYWORDVAR=="AAF26 MAIL ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ACH01 DELL ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="ACZ99 USER",7,	
	ifelse (ETC$KEYWORDVAR=="AAM07 CONFIG COMPUTER",7,	
	ifelse (ETC$KEYWORDVAR=="ABH03 EPDM ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ABQ01 ENGINEER ACESS CONNECT",7,	
	ifelse (ETC$KEYWORDVAR=="ACX99 SERVICE",7,	
	ifelse (ETC$KEYWORDVAR=="AAE14 EMAIL NOTIFICATION",7,	
	ifelse (ETC$KEYWORDVAR=="ABK99 REFUSOL",7,	
	ifelse (ETC$KEYWORDVAR=="ADE10 SCAN ISSUE",7,	
	ifelse (ETC$KEYWORDVAR=="ADQ99 REPORT",7,	
	ifelse (ETC$KEYWORDVAR=="AFY99 CALENDAR",7,	
	ifelse (ETC$KEYWORDVAR=="ADB02 PHONE ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ADV99 PORTAL",8,	
	ifelse (ETC$KEYWORDVAR=="AHH99 FORMAT",8,	
	ifelse (ETC$KEYWORDVAR=="AFX99 CALL",8,	
	ifelse (ETC$KEYWORDVAR=="AHC99 ASSET",8,	
	ifelse (ETC$KEYWORDVAR=="ACR99 SOLIDWORKS",8,	
	ifelse (ETC$KEYWORDVAR=="AAG17 PRINT ZEBRA",8,	
	ifelse (ETC$KEYWORDVAR=="AAC02 WEBEX ACESS",8,	
	ifelse (ETC$KEYWORDVAR=="AFA99 INVOICE",8,	
	ifelse (ETC$KEYWORDVAR=="AAL27 SETUP TS3",8,	
	ifelse (ETC$KEYWORDVAR=="ADE05 SCAN ADMIN",8,	
	ifelse (ETC$KEYWORDVAR=="ABE99 TOOL",8,	
	ifelse (ETC$KEYWORDVAR=="ADT02 INTERFACE DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ACC01 EXCEL ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AGD03 COMPUTER ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAB11 SAP ORDER",8,	
	ifelse (ETC$KEYWORDVAR=="ACM03 SQL ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ACY13 APP ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ABM99 BW",8,	
	ifelse (ETC$KEYWORDVAR=="ABQ99 ENGINEER",8,	
	ifelse (ETC$KEYWORDVAR=="ACH03 DELL ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="ACA99 MS OFFICE",8,	
	ifelse (ETC$KEYWORDVAR=="AAE06 EMAIL SERVER",8,	
	ifelse (ETC$KEYWORDVAR=="AFE99 TAX",8,	
	ifelse (ETC$KEYWORDVAR=="ABN01 BOM ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAL02 SETUP VPN",8,	
	ifelse (ETC$KEYWORDVAR=="AIK99 LOG",8,	
	ifelse (ETC$KEYWORDVAR=="ADA01 NEW HARDWARE",8,	
	ifelse (ETC$KEYWORDVAR=="AAL24 SETUP NETWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ACP03 SITE ISSUE",8,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AAG08 NEW PRINTER",8,	
	ifelse (ETC$KEYWORDVAR=="AAA19 PASSWORD RESET ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="AAO99 VPN",8,	
	ifelse (ETC$KEYWORDVAR=="ACO99 WEBSITE",8,	
	ifelse (ETC$KEYWORDVAR=="AAK12 INSTALL PROJECT",8,	
	ifelse (ETC$KEYWORDVAR=="ABJ99 TS3",8,	
	ifelse (ETC$KEYWORDVAR=="AII99 REFU",8,	
	ifelse (ETC$KEYWORDVAR=="ACK99 CAD",8,	
	ifelse (ETC$KEYWORDVAR=="ABF01 SFDC DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AFF99 BILL",8,	
	ifelse (ETC$KEYWORDVAR=="AEL99 RAM",8,	
	ifelse (ETC$KEYWORDVAR=="AAB23 SAP EXCEL",8,	
	ifelse (ETC$KEYWORDVAR=="AAC03 WEBEX DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AAP02 SERVER DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ADH01 DEBUG REQ",8,	
	ifelse (ETC$KEYWORDVAR=="ACI03 CISCO ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AEX99 SHIP",8,	
	ifelse (ETC$KEYWORDVAR=="ABC99 WIN",8,	
	ifelse (ETC$KEYWORDVAR=="AEA02 LAN DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="ABX99 ADP",8,	
	ifelse (ETC$KEYWORDVAR=="AEM99 MEMORY",8,	
	ifelse (ETC$KEYWORDVAR=="AAS99 SWITCH",8,	
	ifelse (ETC$KEYWORDVAR=="ACJ99 CITRIX",8,	
	ifelse (ETC$KEYWORDVAR=="ADC99 COPIER",8,	
	ifelse (ETC$KEYWORDVAR=="ADE99 SCAN",8,	
	ifelse (ETC$KEYWORDVAR=="AAE07 EMAIL ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="AGI99 HACK",8,	
	ifelse (ETC$KEYWORDVAR=="AAF02 MAIL ACCESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAB15 SAP REPORT",8,	
	ifelse (ETC$KEYWORDVAR=="AHJ99 INVERT",8,	
	ifelse (ETC$KEYWORDVAR=="AAH28 LOGIN CHANGE",8,	
	ifelse (ETC$KEYWORDVAR=="AAT99 NODE",8,	
	ifelse (ETC$KEYWORDVAR=="AEJ99 HEADSET",8,	
	ifelse (ETC$KEYWORDVAR=="ACU02 DRIVE DOWN NOTWORK",8,	
	ifelse (ETC$KEYWORDVAR=="AGA99 BATTERY",8,	
	ifelse (ETC$KEYWORDVAR=="ABL03 ASTEA ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AHG99 FIRM",8,	
	ifelse (ETC$KEYWORDVAR=="AAP09 SERVER ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="AAH51 LOGIN ISSUE TOMCAT",8,	
	ifelse (ETC$KEYWORDVAR=="AAE05 EMAIL INSTALL",8,	
	ifelse (ETC$KEYWORDVAR=="AAL29 SETUP FOPS",8,	
	ifelse (ETC$KEYWORDVAR=="AAK24 INSTALL ENGINEER",8,	
	ifelse (ETC$KEYWORDVAR=="AAG16 PRINT INVOICE",8,	
	ifelse (ETC$KEYWORDVAR=="AHK99 ITEM",8,	
	ifelse (ETC$KEYWORDVAR=="ACW02 SYSTEM ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="ADO99 VOIP",8,	
	ifelse (ETC$KEYWORDVAR=="AAJ07 LOCK PHONE",8,	
	ifelse (ETC$KEYWORDVAR=="ACZ05 NEW HIRE HARDWARE",8,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$KEYWORDVAR_R3 <-	ifelse (ETC$KEYWORDVAR=="AHU99 RECRUIT",8,	
	ifelse (ETC$KEYWORDVAR=="ADS99 HARDWARE",8,	
	ifelse (ETC$KEYWORDVAR=="AET99 EXPENSE",8,	
	ifelse (ETC$KEYWORDVAR=="AHD99 ALERT",8,	
	ifelse (ETC$KEYWORDVAR=="AHB99 DEBUG",8,	
	ifelse (ETC$KEYWORDVAR=="ADG99 CRITICAL",8,	
	ifelse (ETC$KEYWORDVAR=="AAB04 SAP TRAINING",8,	
	ifelse (ETC$KEYWORDVAR=="ABL99 ASTEA",8,	
	ifelse (ETC$KEYWORDVAR=="ADT01 INTERFACE ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AGZ99 DATABASE",8,	
	ifelse (ETC$KEYWORDVAR=="AIJ99 PAY",8,	
	ifelse (ETC$KEYWORDVAR=="AAH01 LOGIN NOT WORK BW",8,	
	ifelse (ETC$KEYWORDVAR=="AAH26 LOGIN LONG",8,	
	ifelse (ETC$KEYWORDVAR=="ADT03 INTERFACE ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAP12 SERVER ISSUE",8,	
	ifelse (ETC$KEYWORDVAR=="AAS01 SWITCH ACESS CONNECT",8,	
	ifelse (ETC$KEYWORDVAR=="AAQ11 NETWORK ACCOUNT",8,	
	ifelse (ETC$KEYWORDVAR=="AAV02 STATION DOWN NOTWORK",8,	paste(ETC$KEYWORDVAR_R3)))))))))))))))))))
	
ETC$KEYWORDVAR_R3 <- ifelse (ETC$KEYWORDVAR_R3 == "NA", 4, paste(ETC$KEYWORDVAR_R3))


###################################################################################################


ETC$Group_O1  <-	ifelse (ETC$Group=="SAP General",1,
	ifelse (ETC$Group=="PCS Tier 1",2,
	ifelse (ETC$Group=="Not Assigned",3,
	ifelse (ETC$Group=="FIS",4,
	ifelse (ETC$Group=="System Support - Do Not Use",5,
	ifelse (ETC$Group=="Facilities",6,
	ifelse (ETC$Group=="SAP-Accounts",7,
	ifelse (ETC$Group=="PCS ENG",8,
	ifelse (ETC$Group=="PCS Tier 2",9,
	ifelse (ETC$Group=="SAP-Security",10,
	ifelse (ETC$Group=="SAP-Service",11,
	ifelse (ETC$Group=="Astea",12,
	ifelse (ETC$Group=="SAP-Germany",13,
	ifelse (ETC$Group=="PCS Tier 3",14,
	ifelse (ETC$Group=="SAP-Sales",15,
	ifelse (ETC$Group=="Sharepoint",16,
	ifelse (ETC$Group=="SAP-ABAP (Do not Use)",17,
	ifelse (ETC$Group=="SAP-Finance",18,
	ifelse (ETC$Group=="Salesforce.com",19,
	ifelse (ETC$Group=="SAP-Operations",20,
	ifelse (ETC$Group=="SAP-Portal",21,
	ifelse (ETC$Group=="PTools",22,
	ifelse (ETC$Group=="SAP-Basis",23,
	ifelse (ETC$Group=="Liferay/SNC",24,
	ifelse (ETC$Group=="SAP-HR",25,
	ifelse (ETC$Group=="Fusion Ops",26,
	ifelse (ETC$Group=="SAP-BW",27,
	ifelse (ETC$Group=="Host Analytics','HostAnalytics",28,
	ifelse (ETC$Group=="Mgmt/Proj Mgmt",29,
	ifelse (ETC$Group=="SAP-APO",30,
	ifelse (ETC$Group=="SAP-PI",31,
	ifelse (ETC$Group=="SAP-PLM",32,9))))))))))))))))))))))))))))))))
	
ETC$Group_O2 <-	ifelse (ETC$Group=="Fusion Ops",1,
	ifelse (ETC$Group=="Facilities",2,
	ifelse (ETC$Group=="PCS Tier 1",3,
	ifelse (ETC$Group=="SAP General",4,
	ifelse (ETC$Group=="System Support - Do Not Use",5,
	ifelse (ETC$Group=="Not Assigned",6,
	ifelse (ETC$Group=="SAP-BW",7,
	ifelse (ETC$Group=="PCS Tier 2",8,
	ifelse (ETC$Group=="Liferay/SNC",9,
	ifelse (ETC$Group=="PCS ENG",10,
	ifelse (ETC$Group=="FIS",11,
	ifelse (ETC$Group=="Sharepoint",12,
	ifelse (ETC$Group=="SAP-Germany",13,
	ifelse (ETC$Group=="SAP-Sales",14,
	ifelse (ETC$Group=="SAP-Security",15,
	ifelse (ETC$Group=="Salesforce.com",16,
	ifelse (ETC$Group=="SAP-ABAP (Do not Use)",17,
	ifelse (ETC$Group=="Astea",18,
	ifelse (ETC$Group=="PTools",19,
	ifelse (ETC$Group=="SAP-Portal",20,
	ifelse (ETC$Group=="SAP-Accounts",21,
	ifelse (ETC$Group=="SAP-Service",22,
	ifelse (ETC$Group=="PCS Tier 3",23,
	ifelse (ETC$Group=="SAP-Finance",24,
	ifelse (ETC$Group=="SAP-Operations",25,
	ifelse (ETC$Group=="SAP-HR",26,
	ifelse (ETC$Group=="SAP-Basis",27,
	5)))))))))))))))))))))))))))
	
ETC$Group_O3 <-	ifelse (ETC$Group=="SAP-Accounts",1,
	ifelse (ETC$Group=="Facilities",2,
	ifelse (ETC$Group=="Not Assigned",3,
	ifelse (ETC$Group=="SAP-Security",4,
	ifelse (ETC$Group=="SAP General",5,
	ifelse (ETC$Group=="SAP-Operations",6,
	ifelse (ETC$Group=="PCS Tier 1",7,
	ifelse (ETC$Group=="System Support - Do Not Use",8,
	ifelse (ETC$Group=="Sharepoint",9,
	ifelse (ETC$Group=="FIS",10,
	ifelse (ETC$Group=="PCS ENG",11,
	ifelse (ETC$Group=="Salesforce.com",12,
	ifelse (ETC$Group=="SAP-Germany",13,
	ifelse (ETC$Group=="PCS Tier 2",14,
	ifelse (ETC$Group=="PCS Tier 3",15,
	ifelse (ETC$Group=="PTools",16,
	ifelse (ETC$Group=="SAP-APO",17,
	ifelse (ETC$Group=="SAP-Basis",18,
	ifelse (ETC$Group=="Fusion Ops",19,
	ifelse (ETC$Group=="Liferay/SNC",20,
	ifelse (ETC$Group=="SAP-Sales",21,
	ifelse (ETC$Group=="SAP-HR",22,
	ifelse (ETC$Group=="Host Analytics','HostAnalytics",23,
	ifelse (ETC$Group=="SAP-Finance",24,
	ifelse (ETC$Group=="SAP-ABAP (Do not Use)",25,
	ifelse (ETC$Group=="SAP-BW",26,
	ifelse (ETC$Group=="SAP-Service",27,
	ifelse (ETC$Group=="SAP-Portal",28,
	ifelse (ETC$Group=="Astea",29,
	ifelse (ETC$Group=="SAP-PI",30,
	ifelse (ETC$Group=="SAP-PLM",31,
	ifelse (ETC$Group=="Mgmt/Proj Mgmt",32,
	8))))))))))))))))))))))))))))))))


ETC$Department_O1 <-	ifelse (ETC$Department=="Precision Power Products Group",1,
	ifelse (ETC$Department=="Global Sales and Marketing",2,
	ifelse (ETC$Department=="Not Assigned",3,
	ifelse (ETC$Department=="CEO Organization",4,
	ifelse (ETC$Department=="Precision Power Products- Engineering",5,
	ifelse (ETC$Department=="Solar Energy BU - Service",6,
	ifelse (ETC$Department=="General",7,
	ifelse (ETC$Department=="Human Resources",8,
	ifelse (ETC$Department=="Inverters Product Group- Engineering",9,
	ifelse (ETC$Department=="Solar Energy BU - Sales Americas",10,
	ifelse (ETC$Department=="Thin Films BU - Engineeri",11,
	ifelse (ETC$Department=="Solar Energy BU - Engineering",12,
	ifelse (ETC$Department=="Global Operations",13,
	ifelse (ETC$Department=="Thin Films BU - Operations",14,
	ifelse (ETC$Department=="Thermal Instrumentation",15,
	ifelse (ETC$Department=="Legal",16,
	ifelse (ETC$Department=="Solar Energy BU - Marketing",17,
	ifelse (ETC$Department=="Thin Films BU - Quality & Reliability",18,
	ifelse (ETC$Department=="Solar Energy BU - Operations",19,
	ifelse (ETC$Department=="Thin Films BU - Engineering",20,
	ifelse (ETC$Department=="Thin Films BU - Marketing",21,
	ifelse (ETC$Department=="Information Technology",22,
	ifelse (ETC$Department=="Internal Audit",23,
	ifelse (ETC$Department=="Finance & Accounting",24,
	ifelse (ETC$Department=="Solar Energy BU",25,
	ifelse (ETC$Department=="Inverters Product Group",26,
	ifelse (ETC$Department=="Thin Films BU",27,
	ifelse (ETC$Department=="Precision Power Products-Sales&Marketing",28,
	ifelse (ETC$Department=="Thin Films BU - Sales",29,
	11)))))))))))))))))))))))))))))
	
ETC$Department_O2 <-	ifelse (ETC$Department=="CEO Organization",1,
	ifelse (ETC$Department=="Not Assigned",2,
	ifelse (ETC$Department=="Inverters Product Group- Engineering",3,
	ifelse (ETC$Department=="Legal",4,
	ifelse (ETC$Department=="Precision Power Products- Engineering",5,
	ifelse (ETC$Department=="Thin Films BU - Quality & Reliability",6,
	ifelse (ETC$Department=="Global Sales and Marketing",7,
	ifelse (ETC$Department=="Thin Films BU - Engineering",8,
	ifelse (ETC$Department=="Human Resources",9,
	ifelse (ETC$Department=="Thin Films BU - Engineeri",10,
	ifelse (ETC$Department=="Solar Energy BU - Sales Americas",11,
	ifelse (ETC$Department=="Thermal Instrumentation",12,
	ifelse (ETC$Department=="Solar Energy BU - Service",13,
	ifelse (ETC$Department=="Thin Films BU - Marketing",14,
	ifelse (ETC$Department=="Precision Power Products Group",15,
	ifelse (ETC$Department=="Solar Energy BU - Marketing",16,
	ifelse (ETC$Department=="Solar Energy BU - Engineering",17,
	ifelse (ETC$Department=="Thin Films BU - Operations",18,
	ifelse (ETC$Department=="Thin Films BU",19,
	ifelse (ETC$Department=="General",20,
	ifelse (ETC$Department=="Global Operations",21,
	ifelse (ETC$Department=="Solar Energy BU - Operations",22,
	ifelse (ETC$Department=="Information Technology",23,
	ifelse (ETC$Department=="Solar Energy BU",24,
	ifelse (ETC$Department=="Finance & Accounting",25,
	ifelse (ETC$Department=="Internal Audit",26,
	ifelse (ETC$Department=="Inverters Product Group",27,
	14)))))))))))))))))))))))))))
	
ETC$Department_O3 <-	ifelse (ETC$Department=="Inverters Product Group",1,
	ifelse (ETC$Department=="General",2,
	ifelse (ETC$Department=="Global Sales and Marketing",3,
	ifelse (ETC$Department=="Thin Films BU - Sales",4,
	ifelse (ETC$Department=="Precision Power Products-Sales&Marketing",5,
	ifelse (ETC$Department=="Thermal Instrumentation",6,
	ifelse (ETC$Department=="Internal Audit",7,
	ifelse (ETC$Department=="Solar Energy BU",8,
	ifelse (ETC$Department=="Solar Energy BU - Sales Americas",9,
	ifelse (ETC$Department=="Solar Energy BU - Operations",10,
	ifelse (ETC$Department=="Thin Films BU - Engineeri",11,
	ifelse (ETC$Department=="Global Operations",12,
	ifelse (ETC$Department=="Thin Films BU - Operations",13,
	ifelse (ETC$Department=="Precision Power Products- Engineering",14,
	ifelse (ETC$Department=="Solar Energy BU - Service",15,
	ifelse (ETC$Department=="Human Resources",16,
	ifelse (ETC$Department=="Inverters Product Group- Engineering",17,
	ifelse (ETC$Department=="Solar Energy BU - Marketing",18,
	ifelse (ETC$Department=="Information Technology",19,
	ifelse (ETC$Department=="Thin Films BU - Engineering",20,
	ifelse (ETC$Department=="Not Assigned",21,
	ifelse (ETC$Department=="Thin Films BU - Marketing",22,
	ifelse (ETC$Department=="Solar Energy BU - Engineering",23,
	ifelse (ETC$Department=="Thin Films BU",24,
	ifelse (ETC$Department=="Finance & Accounting",25,
	ifelse (ETC$Department=="CEO Organization",26,
	ifelse (ETC$Department=="Thin Films BU - Quality & Reliability",27,
	ifelse (ETC$Department=="Legal",28,
	21))))))))))))))))))))))))))))


ETC$Region <- ifelse(ETC$Site=="Austin" | ETC$Site=="Georgetown" | ETC$Site=="Bend" |
                     ETC$Site=="Fort Collins" | ETC$Site=="Fort Collins" | ETC$Site=="CO','FortCollins" |
                     ETC$Site=="Mississauga" | ETC$Site=="San Jose" | ETC$Site=="Ronkonkoma" |
                     ETC$Site=="New York" | ETC$Site=="Toronto" | ETC$Site=="Vancouver"
                     ,"Americas",
              ifelse(ETC$Site=="Belecke" | ETC$Site=="Bicester" |
                     ETC$Site=="Filderstadt (Bonlanden)" | ETC$Site=="Littlehampton" |
                     ETC$Site=="Metzingen" | ETC$Site=="Villaz-St-Pierre"
                     ,"Europe",     
              ifelse(ETC$Site=="Chiyoda-Ku" | ETC$Site=="Chiyoda-ku" | ETC$Site=="Tokyo" |
                     ETC$Site=="Chungho City" | ETC$Site=="Taipei Hsie" | ETC$Site=="Fujisawa-Shi" |
                     ETC$Site=="Hachioji" | ETC$Site=="Korea" | ETC$Site=="Pune" |
                     ETC$Site=="Seoul" | ETC$Site=="Shanghai" | ETC$Site=="Shenzhen" | ETC$Site=="Singapore"
                     ,"Asia",
              ifelse(ETC$Site=="Data Center" | ETC$Site=="Not Assigned" | ETC$Site=="SNC","Not Assigned", "Not Assigned"))))

ETC$Region_O1 <-	ifelse (ETC$Region=="Not Assigned",1,
	ifelse (ETC$Region=="Americas",2,
	ifelse (ETC$Region=="Europe",3,
	ifelse (ETC$Region=="Asia",4,
	3))))
	
ETC$Region_O2	<- ifelse (ETC$Region=="Americas",1,
	ifelse (ETC$Region=="Not Assigned",2,
	ifelse (ETC$Region=="Asia",3,
	ifelse (ETC$Region=="Europe",4,
	2))))
	
ETC$Region_O3	<- ifelse (ETC$Region=="Asia",1,
	ifelse (ETC$Region=="Europe",2,
	ifelse (ETC$Region=="Americas",3,
	ifelse (ETC$Region=="Not Assigned",4,
	3))))


ETC$RequestType_O1 <-	ifelse (ETC$RequestType=="Informational",1,
	ifelse (ETC$RequestType=="Access/Security Request",2,
	ifelse (ETC$RequestType=="Service Request",3,
	ifelse (ETC$RequestType=="Incident",4,
	ifelse (ETC$RequestType=="Not Assigned",5,
	5)))))
	
ETC$RequestType_O2 <-	ifelse (ETC$RequestType=="Informational",1,
	ifelse (ETC$RequestType=="Access/Security Request",2,
	ifelse (ETC$RequestType=="Service Request",3,
	ifelse (ETC$RequestType=="Incident",4,
	4))))
	
ETC$RequestType_O3 <-	ifelse (ETC$RequestType=="Access/Security Request",1,
	ifelse (ETC$RequestType=="Incident",2,
	ifelse (ETC$RequestType=="Service Request",3,
	ifelse (ETC$RequestType=="Informational",4,
	ifelse (ETC$RequestType=="Not Assigned",5,
	5)))))


###################################################################################################

ETC$Category_O1 <-	ifelse (ETC$Category=="Janitorial",1,
	ifelse (ETC$Category=="Equipment",2,
	ifelse (ETC$Category=="Software (non-ENG)",3,
	ifelse (ETC$Category=="Telecom",4,
	ifelse (ETC$Category=="Security",5,
	ifelse (ETC$Category=="Server",6,
	ifelse (ETC$Category=="Hardware",7,
	ifelse (ETC$Category=="Other",8,
	ifelse (ETC$Category=="Software (ENG)",9,
	ifelse (ETC$Category=="Furniture",10,
	ifelse (ETC$Category=="Enterprise Solutions",11,
	ifelse (ETC$Category=="Conference Room",12,
	ifelse (ETC$Category=="Electrical",13,
	ifelse (ETC$Category=="Not Assigned",14,
	8))))))))))))))


ETC$Subcategory_O1 <- "NA"

ETC$Subcategory_O1 <-	ifelse (ETC$Subcategory=="Adobe Illustrator",1,	
	ifelse (ETC$Subcategory=="Citrix",2,	
	ifelse (ETC$Subcategory=="Install",3,	
	ifelse (ETC$Subcategory=="MS One Note",4,	
	ifelse (ETC$Subcategory=="Maintenance",5,	
	ifelse (ETC$Subcategory=="PDMWorks",6,	
	ifelse (ETC$Subcategory=="Project Web Access",7,	
	ifelse (ETC$Subcategory=="Remove",8,	
	ifelse (ETC$Subcategory=="SAP-Firefight/CheckoutID",9,	
	ifelse (ETC$Subcategory=="SCCM/MDT/WSUS",10,	
	ifelse (ETC$Subcategory=="WebEx Connect','Webex Connect",11,	
	ifelse (ETC$Subcategory=="WebEx Conferencing','Webex Conferencing",12,	
	ifelse (ETC$Subcategory=="Remote Access",13,	
	ifelse (ETC$Subcategory=="Wireless",14,	
	ifelse (ETC$Subcategory=="Active Directory",15,	
	ifelse (ETC$Subcategory=="Purchase Card (ProCard)",16,	
	ifelse (ETC$Subcategory=="Active HDL",17,	
	ifelse (ETC$Subcategory=="MS Outlook",18,	
	ifelse (ETC$Subcategory=="Storage System",19,	
	ifelse (ETC$Subcategory=="Other Smart Phone",20,	
	ifelse (ETC$Subcategory=="Mouse",21,	
	ifelse (ETC$Subcategory=="Phone - VoIP",22,	
	ifelse (ETC$Subcategory=="VPN/Remote Access",23,	
	ifelse (ETC$Subcategory=="Windows Firewall (only Software)",24,	
	ifelse (ETC$Subcategory=="iPhone",25,	
	ifelse (ETC$Subcategory=="Source Gear",26,	
	ifelse (ETC$Subcategory=="Cisco Deskphone (only Software)",27,	
	ifelse (ETC$Subcategory=="Internet Explorer",28,	
	ifelse (ETC$Subcategory=="VPN (only Software)",29,	
	ifelse (ETC$Subcategory=="Printer (only Software)",30,	
	ifelse (ETC$Subcategory=="MS PowerPoint",31,	
	ifelse (ETC$Subcategory=="OS Win2K3",32,	
	ifelse (ETC$Subcategory=="Policy",33,	
	ifelse (ETC$Subcategory=="Tablet",34,	
	ifelse (ETC$Subcategory=="MS Project",35,	
	ifelse (ETC$Subcategory=="MS Visio",36,	
	ifelse (ETC$Subcategory=="Email/Exchange",37,	
	ifelse (ETC$Subcategory=="Service Desk",38,	
	ifelse (ETC$Subcategory=="Adobe",39,	
	ifelse (ETC$Subcategory=="Peripheral",40,	
	ifelse (ETC$Subcategory=="MS Excel",41,	
	ifelse (ETC$Subcategory=="Application (Other)','Application (other)",42,	
	ifelse (ETC$Subcategory=="Exchange",43,	
	ifelse (ETC$Subcategory=="FIS",44,	
	ifelse (ETC$Subcategory=="File Server",45,	
	ifelse (ETC$Subcategory=="SAP-Security",46,	
	ifelse (ETC$Subcategory=="OS WIN XP",47,	
	ifelse (ETC$Subcategory=="OS Win7",48,	paste(ETC$Subcategory_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O1 <-	ifelse (ETC$Subcategory=="Alert/Monitoring",49,	
	ifelse (ETC$Subcategory=="ED for Windows",50,	
	ifelse (ETC$Subcategory=="File Transfer",51,	
	ifelse (ETC$Subcategory=="OS (Other)",52,	
	ifelse (ETC$Subcategory=="OS MAC OS",53,	
	ifelse (ETC$Subcategory=="OS/Appication Apple IOS",54,	
	ifelse (ETC$Subcategory=="OS/Application Other Mobile",55,	
	ifelse (ETC$Subcategory=="Other Browser",56,	
	ifelse (ETC$Subcategory=="PCAD (Network Schematic)",57,	
	ifelse (ETC$Subcategory=="Shared Folder/Drive",58,	
	ifelse (ETC$Subcategory=="VMWare",59,	
	ifelse (ETC$Subcategory=="Printer",60,	
	ifelse (ETC$Subcategory=="SAP-Accounts",61,	
	ifelse (ETC$Subcategory=="Domain/WebSite URL",62,	
	ifelse (ETC$Subcategory=="Virus / Malware (only Software)",63,	
	ifelse (ETC$Subcategory=="EPDM (Enterprise Product Data Mgmt)",64,	
	ifelse (ETC$Subcategory=="Keyboard",65,	
	ifelse (ETC$Subcategory=="Backup",66,	
	ifelse (ETC$Subcategory=="Network Device (Other)",67,	
	ifelse (ETC$Subcategory=="ISP (WAN / DIA)",68,	
	ifelse (ETC$Subcategory=="Projector",69,	
	ifelse (ETC$Subcategory=="Matlab",70,	
	ifelse (ETC$Subcategory=="Preventative Maintenance",71,	
	ifelse (ETC$Subcategory=="Desktop",72,	
	ifelse (ETC$Subcategory=="Ptools",73,	
	ifelse (ETC$Subcategory=="AD Services (DNS,WINS,DHCP,NPS)",74,	
	ifelse (ETC$Subcategory=="Astea",75,	
	ifelse (ETC$Subcategory=="Hardware (Other)",76,	
	ifelse (ETC$Subcategory=="Solid Works",77,	
	ifelse (ETC$Subcategory=="MathCAD",78,	
	ifelse (ETC$Subcategory=="Docking Station",79,	
	ifelse (ETC$Subcategory=="Other",80,	
	ifelse (ETC$Subcategory=="Fax (only Software)",81,	
	ifelse (ETC$Subcategory=="MS Word",82,	
	ifelse (ETC$Subcategory=="OS Other",83,	
	ifelse (ETC$Subcategory=="OS Win 2K8",84,	
	ifelse (ETC$Subcategory=="Laptop",85,	
	ifelse (ETC$Subcategory=="Server",86,	
	ifelse (ETC$Subcategory=="SharePoint",87,	
	ifelse (ETC$Subcategory=="SAP",88,	
	ifelse (ETC$Subcategory=="Altium Designer",89,	
	ifelse (ETC$Subcategory=="Broken",90,	
	ifelse (ETC$Subcategory=="Client Backup",91,	
	ifelse (ETC$Subcategory=="Reconfigure",92,	
	ifelse (ETC$Subcategory=="UPS",93,	
	ifelse (ETC$Subcategory=="SQL Server",94,	
	ifelse (ETC$Subcategory=="Laptop Battery",95,	
	ifelse (ETC$Subcategory=="SFDC",96,	paste(ETC$Subcategory_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O1 <-	ifelse (ETC$Subcategory=="Switch",97,	
	ifelse (ETC$Subcategory=="Firewall/VPN",98,	
	ifelse (ETC$Subcategory=="Liferay",99,	
	ifelse (ETC$Subcategory=="Host Analytics",100,	
	ifelse (ETC$Subcategory=="2DEditor",101,	
	ifelse (ETC$Subcategory=="A/V",102,	
	ifelse (ETC$Subcategory=="Analyst",103,	
	ifelse (ETC$Subcategory=="BI",104,	
	ifelse (ETC$Subcategory=="Blackberry",105,	
	ifelse (ETC$Subcategory=="CAM 350",106,	
	ifelse (ETC$Subcategory=="ECAD (Electronic CAD)",107,	
	ifelse (ETC$Subcategory=="Embarcadero C++ Builder",108,	
	ifelse (ETC$Subcategory=="Firewall",109,	
	ifelse (ETC$Subcategory=="Green Hills",110,	
	ifelse (ETC$Subcategory=="IBM Rational - ClearQuest",111,	
	ifelse (ETC$Subcategory=="LabView",112,	
	ifelse (ETC$Subcategory=="MDT Image",113,	
	ifelse (ETC$Subcategory=="New",114,	
	ifelse (ETC$Subcategory=="Not Assigned",115,	
	ifelse (ETC$Subcategory=="PCAD (PCB Layout)",116,	
	ifelse (ETC$Subcategory=="Router",117,	
	ifelse (ETC$Subcategory=="Routing",118,	
	ifelse (ETC$Subcategory=="Setup",119,	
	ifelse (ETC$Subcategory=="Sonnet Microwave Suite",120,	
	ifelse (ETC$Subcategory=="Timming Designer",121,	paste(ETC$Subcategory_O1))))))))))))))))))))))))))
	        
ETC$Subcategory_O1 <- ifelse (ETC$Subcategory_O1 == "NA",80, paste(ETC$Subcategory_O1))	
		
ETC$Subcategory_O2 <- "NA"

ETC$Subcategory_O2 <-	ifelse (ETC$Subcategory=="MS One Note",1,	
	ifelse (ETC$Subcategory=="Adobe Illustrator",2,	
	ifelse (ETC$Subcategory=="Client Backup",3,	
	ifelse (ETC$Subcategory=="OS Other",4,	
	ifelse (ETC$Subcategory=="Project Web Access",5,	
	ifelse (ETC$Subcategory=="Maintenance",6,	
	ifelse (ETC$Subcategory=="Host Analytics",7,	
	ifelse (ETC$Subcategory=="Install",8,	
	ifelse (ETC$Subcategory=="OS Win2K3",9,	
	ifelse (ETC$Subcategory=="SAP-Firefight/CheckoutID",10,	
	ifelse (ETC$Subcategory=="OS MAC OS",11,	
	ifelse (ETC$Subcategory=="Tablet",12,	
	ifelse (ETC$Subcategory=="PCAD (Network Schematic)",13,	
	ifelse (ETC$Subcategory=="Firewall/VPN",14,	
	ifelse (ETC$Subcategory=="Remove",15,	
	ifelse (ETC$Subcategory=="Keyboard",16,	
	ifelse (ETC$Subcategory=="MathCAD",17,	
	ifelse (ETC$Subcategory=="Webex Conferencing','WebEx Conferencing",18,	
	ifelse (ETC$Subcategory=="Wireless",19,	
	ifelse (ETC$Subcategory=="Policy",20,	
	ifelse (ETC$Subcategory=="MS Word",21,	
	ifelse (ETC$Subcategory=="Active Directory",22,	
	ifelse (ETC$Subcategory=="Other Browser",23,	
	ifelse (ETC$Subcategory=="Purchase Card (ProCard)",24,	
	ifelse (ETC$Subcategory=="Webex Connect','WebEx Connect",25,	
	ifelse (ETC$Subcategory=="File Transfer",26,	
	ifelse (ETC$Subcategory=="ED for Windows",27,	
	ifelse (ETC$Subcategory=="MS Outlook",28,	
	ifelse (ETC$Subcategory=="MS PowerPoint",29,	
	ifelse (ETC$Subcategory=="Preventative Maintenance",30,	
	ifelse (ETC$Subcategory=="VPN/Remote Access",31,	
	ifelse (ETC$Subcategory=="OS (Other)",32,	
	ifelse (ETC$Subcategory=="SQL Server",33,	
	ifelse (ETC$Subcategory=="Adobe",34,	
	ifelse (ETC$Subcategory=="VPN (only Software)",35,	
	ifelse (ETC$Subcategory=="MS Visio",36,	
	ifelse (ETC$Subcategory=="Phone - VoIP",37,	
	ifelse (ETC$Subcategory=="Remote Access",38,	
	ifelse (ETC$Subcategory=="Domain/WebSite URL",39,	
	ifelse (ETC$Subcategory=="Desktop",40,	
	ifelse (ETC$Subcategory=="Laptop",41,	
	ifelse (ETC$Subcategory=="Network Device (Other)",42,	
	ifelse (ETC$Subcategory=="Service Desk",43,	
	ifelse (ETC$Subcategory=="Solid Works",44,	
	ifelse (ETC$Subcategory=="Source Gear",45,	
	ifelse (ETC$Subcategory=="MS Excel",46,	
	ifelse (ETC$Subcategory=="Application (Other)','Application (other)",47,	
	ifelse (ETC$Subcategory=="iPhone",48,	paste(ETC$Subcategory_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O2 <-	ifelse (ETC$Subcategory=="Cisco Deskphone (only Software)",49,	
	ifelse (ETC$Subcategory=="Exchange",50,	
	ifelse (ETC$Subcategory=="Internet Explorer",51,	
	ifelse (ETC$Subcategory=="MS Project",52,	
	ifelse (ETC$Subcategory=="Printer (only Software)",53,	
	ifelse (ETC$Subcategory=="AD Services (DNS,WINS,DHCP,NPS)",54,	
	ifelse (ETC$Subcategory=="Email/Exchange",55,	
	ifelse (ETC$Subcategory=="SharePoint",56,	
	ifelse (ETC$Subcategory=="Windows Firewall (only Software)",57,	
	ifelse (ETC$Subcategory=="PDMWorks",58,	
	ifelse (ETC$Subcategory=="SAP-Security",59,	
	ifelse (ETC$Subcategory=="Reconfigure",60,	
	ifelse (ETC$Subcategory=="Virus / Malware (only Software)",61,	
	ifelse (ETC$Subcategory=="Printer",62,	
	ifelse (ETC$Subcategory=="Broken",63,	
	ifelse (ETC$Subcategory=="Docking Station",64,	
	ifelse (ETC$Subcategory=="ISP (WAN / DIA)",65,	
	ifelse (ETC$Subcategory=="OS WIN XP",66,	
	ifelse (ETC$Subcategory=="Other Smart Phone",67,	
	ifelse (ETC$Subcategory=="Shared Folder/Drive",68,	
	ifelse (ETC$Subcategory=="Alert/Monitoring",69,	
	ifelse (ETC$Subcategory=="Ptools",70,	
	ifelse (ETC$Subcategory=="OS Win 2K8",71,	
	ifelse (ETC$Subcategory=="Astea",72,	
	ifelse (ETC$Subcategory=="Switch",73,	
	ifelse (ETC$Subcategory=="File Server",74,	
	ifelse (ETC$Subcategory=="OS Win7",75,	
	ifelse (ETC$Subcategory=="Peripheral",76,	
	ifelse (ETC$Subcategory=="SAP-Accounts",77,	
	ifelse (ETC$Subcategory=="FIS",78,	
	ifelse (ETC$Subcategory=="Projector",79,	
	ifelse (ETC$Subcategory=="SFDC",80,	
	ifelse (ETC$Subcategory=="UPS",81,	
	ifelse (ETC$Subcategory=="Backup",82,	
	ifelse (ETC$Subcategory=="SAP",83,	
	ifelse (ETC$Subcategory=="EPDM (Enterprise Product Data Mgmt)",84,	
	ifelse (ETC$Subcategory=="Storage System",85,	
	ifelse (ETC$Subcategory=="Laptop Battery",86,	
	ifelse (ETC$Subcategory=="Active HDL",87,	
	ifelse (ETC$Subcategory=="OS/Application Other Mobile",88,	
	ifelse (ETC$Subcategory=="Liferay",89,	
	ifelse (ETC$Subcategory=="Mouse",90,	
	ifelse (ETC$Subcategory=="Hardware (Other)",91,	
	ifelse (ETC$Subcategory=="Matlab",92,	
	ifelse (ETC$Subcategory=="Other",93,	
	ifelse (ETC$Subcategory=="Altium Designer",94,	
	ifelse (ETC$Subcategory=="OS/Appication Apple IOS",95,	
	ifelse (ETC$Subcategory=="Fax (only Software)",96,	paste(ETC$Subcategory_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O2 <-	ifelse (ETC$Subcategory=="VMWare",97,	
	ifelse (ETC$Subcategory=="Server",98,	
	ifelse (ETC$Subcategory=="SCCM/MDT/WSUS",99,	
	ifelse (ETC$Subcategory=="Citrix",100,	paste(ETC$Subcategory_O2)))))

ETC$Subcategory_O2 <- ifelse (ETC$Subcategory_O2 == "NA",93, paste(ETC$Subcategory_O2))	

ETC$Subcategory_O3 <- "NA"
		
ETC$Subcategory_O3 <-	ifelse (ETC$Subcategory=="Policy",1,	
	ifelse (ETC$Subcategory=="OS MAC OS",2,	
	ifelse (ETC$Subcategory=="Setup",3,	
	ifelse (ETC$Subcategory=="Purchase Card (ProCard)",4,	
	ifelse (ETC$Subcategory=="Timming Designer",5,	
	ifelse (ETC$Subcategory=="OS/Appication Apple IOS",6,	
	ifelse (ETC$Subcategory=="PCAD (PCB Layout)",7,	
	ifelse (ETC$Subcategory=="Sonnet Microwave Suite",8,	
	ifelse (ETC$Subcategory=="Tablet",9,	
	ifelse (ETC$Subcategory=="Analyst",10,	
	ifelse (ETC$Subcategory=="Fax (only Software)",11,	
	ifelse (ETC$Subcategory=="OS/Application Other Mobile",12,	
	ifelse (ETC$Subcategory=="Broken",13,	
	ifelse (ETC$Subcategory=="File Transfer",14,	
	ifelse (ETC$Subcategory=="Other Browser",15,	
	ifelse (ETC$Subcategory=="Routing",16,	
	ifelse (ETC$Subcategory=="OS Win2K3",17,	
	ifelse (ETC$Subcategory=="Reconfigure",18,	
	ifelse (ETC$Subcategory=="Storage System",19,	
	ifelse (ETC$Subcategory=="MS PowerPoint",20,	
	ifelse (ETC$Subcategory=="CAM 350",21,	
	ifelse (ETC$Subcategory=="Other Smart Phone",22,	
	ifelse (ETC$Subcategory=="Wireless",23,	
	ifelse (ETC$Subcategory=="Printer (only Software)",24,	
	ifelse (ETC$Subcategory=="iPhone",25,	
	ifelse (ETC$Subcategory=="Active HDL",26,	
	ifelse (ETC$Subcategory=="PCAD (Network Schematic)",27,	
	ifelse (ETC$Subcategory=="SAP-Accounts",28,	
	ifelse (ETC$Subcategory=="Altium Designer",29,	
	ifelse (ETC$Subcategory=="OS Win 2K8",30,	
	ifelse (ETC$Subcategory=="ECAD (Electronic CAD)",31,	
	ifelse (ETC$Subcategory=="MS Project",32,	
	ifelse (ETC$Subcategory=="Backup",33,	
	ifelse (ETC$Subcategory=="SAP-Security",34,	
	ifelse (ETC$Subcategory=="Alert/Monitoring",35,	
	ifelse (ETC$Subcategory=="Internet Explorer",36,	
	ifelse (ETC$Subcategory=="Active Directory",37,	
	ifelse (ETC$Subcategory=="Remote Access",38,	
	ifelse (ETC$Subcategory=="OS (Other)",39,	
	ifelse (ETC$Subcategory=="SharePoint",40,	
	ifelse (ETC$Subcategory=="MS Excel",41,	
	ifelse (ETC$Subcategory=="OS WIN XP",42,	
	ifelse (ETC$Subcategory=="Shared Folder/Drive",43,	
	ifelse (ETC$Subcategory=="EPDM (Enterprise Product Data Mgmt)",44,	
	ifelse (ETC$Subcategory=="Email/Exchange",45,	
	ifelse (ETC$Subcategory=="WebEx Conferencing','Webex Conferencing",46,	
	ifelse (ETC$Subcategory=="VPN (only Software)",47,	
	ifelse (ETC$Subcategory=="OS Other",48,	paste(ETC$Subcategory_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O3 <-	ifelse (ETC$Subcategory=="Mouse",49,	
	ifelse (ETC$Subcategory=="Hardware (Other)",50,	
	ifelse (ETC$Subcategory=="Adobe",51,	
	ifelse (ETC$Subcategory=="LabView",52,	
	ifelse (ETC$Subcategory=="MS Word",53,	
	ifelse (ETC$Subcategory=="Cisco Deskphone (only Software)",54,	
	ifelse (ETC$Subcategory=="File Server",55,	
	ifelse (ETC$Subcategory=="Domain/WebSite URL",56,	
	ifelse (ETC$Subcategory=="Client Backup",57,	
	ifelse (ETC$Subcategory=="Printer",58,	
	ifelse (ETC$Subcategory=="MS Outlook",59,	
	ifelse (ETC$Subcategory=="SFDC",60,	
	ifelse (ETC$Subcategory=="Service Desk",61,	
	ifelse (ETC$Subcategory=="Solid Works",62,	
	ifelse (ETC$Subcategory=="Application (Other)','Application (other)",63,	
	ifelse (ETC$Subcategory=="Virus / Malware (only Software)",64,	
	ifelse (ETC$Subcategory=="New",65,	
	ifelse (ETC$Subcategory=="OS Win7",66,	
	ifelse (ETC$Subcategory=="Ptools",67,	
	ifelse (ETC$Subcategory=="FIS",68,	
	ifelse (ETC$Subcategory=="Network Device (Other)",69,	
	ifelse (ETC$Subcategory=="MDT Image",70,	
	ifelse (ETC$Subcategory=="AD Services (DNS,WINS,DHCP,NPS)",71,	
	ifelse (ETC$Subcategory=="Phone - VoIP",72,	
	ifelse (ETC$Subcategory=="Keyboard",73,	
	ifelse (ETC$Subcategory=="Exchange",74,	
	ifelse (ETC$Subcategory=="Projector",75,	
	ifelse (ETC$Subcategory=="Server",76,	
	ifelse (ETC$Subcategory=="ISP (WAN / DIA)",77,	
	ifelse (ETC$Subcategory=="MS Visio",78,	
	ifelse (ETC$Subcategory=="Router",79,	
	ifelse (ETC$Subcategory=="Docking Station",80,	
	ifelse (ETC$Subcategory=="SAP",81,	
	ifelse (ETC$Subcategory=="Firewall",82,	
	ifelse (ETC$Subcategory=="Host Analytics",83,	
	ifelse (ETC$Subcategory=="Embarcadero C++ Builder",84,	
	ifelse (ETC$Subcategory=="Preventative Maintenance",85,	
	ifelse (ETC$Subcategory=="SQL Server",86,	
	ifelse (ETC$Subcategory=="Peripheral",87,	
	ifelse (ETC$Subcategory=="Source Gear",88,	
	ifelse (ETC$Subcategory=="VPN/Remote Access",89,	
	ifelse (ETC$Subcategory=="UPS",90,	
	ifelse (ETC$Subcategory=="MathCAD",91,	
	ifelse (ETC$Subcategory=="Windows Firewall (only Software)",92,	
	ifelse (ETC$Subcategory=="Matlab",93,	
	ifelse (ETC$Subcategory=="Desktop",94,	
	ifelse (ETC$Subcategory=="WebEx Connect','Webex Connect",95,	
	ifelse (ETC$Subcategory=="Laptop",96,	paste(ETC$Subcategory_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Subcategory_O3 <-	ifelse (ETC$Subcategory=="Other",97,	
	ifelse (ETC$Subcategory=="Switch",98,	
	ifelse (ETC$Subcategory=="Liferay",99,	
	ifelse (ETC$Subcategory=="Firewall/VPN",100,	
	ifelse (ETC$Subcategory=="Laptop Battery",101,	
	ifelse (ETC$Subcategory=="ED for Windows",102,	
	ifelse (ETC$Subcategory=="Blackberry",103,	
	ifelse (ETC$Subcategory=="A/V",104,	
	ifelse (ETC$Subcategory=="VMWare",105,	
	ifelse (ETC$Subcategory=="Astea",106,	
	ifelse (ETC$Subcategory=="BI",107,	
	ifelse (ETC$Subcategory=="IBM Rational - ClearQuest",108,	
	ifelse (ETC$Subcategory=="2DEditor",109,	
	ifelse (ETC$Subcategory=="Green Hills",110,	
	ifelse (ETC$Subcategory=="Not Assigned",111,	paste(ETC$Subcategory_O3))))))))))))))))

ETC$Subcategory_O3 <- ifelse (ETC$Subcategory_O3 == "NA",97, paste(ETC$Subcategory_O3))	



ETC$Item_O1 <- "NA"

ETC$Item_O1 <-	ifelse (ETC$Item=="Add",1,	
	ifelse (ETC$Item=="Air freshener",2,	
	ifelse (ETC$Item=="Chair",3,	
	ifelse (ETC$Item=="Checkout & Firefighter ID",4,	
	ifelse (ETC$Item=="Error Selection",5,	
	ifelse (ETC$Item=="Open Client Request",6,	
	ifelse (ETC$Item=="Provision/Remove Application",7,	
	ifelse (ETC$Item=="Voicemail - Remove",8,	
	ifelse (ETC$Item=="Wet floor",9,	
	ifelse (ETC$Item=="Remove Account",10,	
	ifelse (ETC$Item=="Remove",11,	
	ifelse (ETC$Item=="Remove configuration",12,	
	ifelse (ETC$Item=="Voicemail password reset",13,	
	ifelse (ETC$Item=="Lock Out / Password Reset','Lock Out/Password Reset",14,	
	ifelse (ETC$Item=="Lock Out/Password Reset (Tier 1 for all but HR)",15,	
	ifelse (ETC$Item=="Password Reset",16,	
	ifelse (ETC$Item=="Create Account/Object",17,	
	ifelse (ETC$Item=="SAP Logon pad replace",18,	
	ifelse (ETC$Item=="Disable",19,	
	ifelse (ETC$Item=="Spam",20,	
	ifelse (ETC$Item=="CPU LOAD",21,	
	ifelse (ETC$Item=="How-To','How To",22,	
	ifelse (ETC$Item=="Update",23,	
	ifelse (ETC$Item=="PC Enable",24,	
	ifelse (ETC$Item=="Agent Deploy/Configure",25,	
	ifelse (ETC$Item=="Disk Failure",26,	
	ifelse (ETC$Item=="Add to Domain",27,	
	ifelse (ETC$Item=="Setup/Configuration",28,	
	ifelse (ETC$Item=="Record Retention - Error",29,	
	ifelse (ETC$Item=="Call Forwarding - Enable",30,	
	ifelse (ETC$Item=="Distribution List (add/del/modify)",31,	
	ifelse (ETC$Item=="Voicemail - How To",32,	
	ifelse (ETC$Item=="Access",33,	
	ifelse (ETC$Item=="Email address Setup",34,	
	ifelse (ETC$Item=="Admin Access",35,	
	ifelse (ETC$Item=="Grant special Permissions",36,	
	ifelse (ETC$Item=="Block",37,	
	ifelse (ETC$Item=="Phone",38,	
	ifelse (ETC$Item=="Physical Memory",39,	
	ifelse (ETC$Item=="Request",40,	
	ifelse (ETC$Item=="User (add/del/modify)",41,	
	ifelse (ETC$Item=="SAP GUI Instalation",42,	
	ifelse (ETC$Item=="Email address Change/Modify",43,	
	ifelse (ETC$Item=="Configure/Setup",44,	
	ifelse (ETC$Item=="Issue/Error",45,	
	ifelse (ETC$Item=="Create Account",46,	
	ifelse (ETC$Item=="Install/Purchase",47,	
	ifelse (ETC$Item=="Modify",48,	paste(ETC$Item_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O1 <-	ifelse (ETC$Item=="Modify configuration",49,	
	ifelse (ETC$Item=="Add/Remove Group",50,	
	ifelse (ETC$Item=="Mailbox Full",51,	
	ifelse (ETC$Item=="Error",52,	
	ifelse (ETC$Item=="Configuration change (non-OS)",53,	
	ifelse (ETC$Item=="New/Replace",54,	
	ifelse (ETC$Item=="Not Assigned",55,	
	ifelse (ETC$Item=="Replace",56,	
	ifelse (ETC$Item=="Access Request",57,	
	ifelse (ETC$Item=="System clean/repair",58,	
	ifelse (ETC$Item=="Add/Remove Technician",59,	
	ifelse (ETC$Item=="Node Reboot",60,	
	ifelse (ETC$Item=="Unblock",61,	
	ifelse (ETC$Item=="Change Account",62,	
	ifelse (ETC$Item=="Node Down",63,	
	ifelse (ETC$Item=="Loaner Request",64,	
	ifelse (ETC$Item=="Site add/remove",65,	
	ifelse (ETC$Item=="Upgrade",66,	
	ifelse (ETC$Item=="Volume Capacity",67,	
	ifelse (ETC$Item=="Maintenance",68,	
	ifelse (ETC$Item=="Setup/Upgrade",69,	
	ifelse (ETC$Item=="Upgrade/Update",70,	
	ifelse (ETC$Item=="Configuration",71,	
	ifelse (ETC$Item=="Failure",72,	
	ifelse (ETC$Item=="File restore",73,	
	ifelse (ETC$Item=="Add Category/Sub-category/Item",74,	
	ifelse (ETC$Item=="Break/Fix",75,	
	ifelse (ETC$Item=="Call Forwarding - Disable",76,	
	ifelse (ETC$Item=="Cubicle",77,	
	ifelse (ETC$Item=="Decomission",78,	
	ifelse (ETC$Item=="Restore",79,	
	ifelse (ETC$Item=="Role Change",80,	
	ifelse (ETC$Item=="Manager change/Cost center change",81,	
	ifelse (ETC$Item=="New configuration",82,	
	ifelse (ETC$Item=="Customer request (non-Security)",83,	
	ifelse (ETC$Item=="Error/Failure",84,	
	ifelse (ETC$Item=="Role Assign",85,	
	ifelse (ETC$Item=="Other",86,	
	ifelse (ETC$Item=="Information Request",87,	
	ifelse (ETC$Item=="Install",88,	
	ifelse (ETC$Item=="Printer Issue",89,	
	ifelse (ETC$Item=="Add/remove access",90,	
	ifelse (ETC$Item=="New/Purchase",91,	
	ifelse (ETC$Item=="Firewall change rule/setting",92,	
	ifelse (ETC$Item=="Firewall remove rule/setting",93,	
	ifelse (ETC$Item=="Restore from archive",94,	
	ifelse (ETC$Item=="Configuration change",95,	
	ifelse (ETC$Item=="Role Create",96,	paste(ETC$Item_O1)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O1 <-	ifelse (ETC$Item=="Debug Request",97,	
	ifelse (ETC$Item=="Firewall add rule/setting",98,	
	ifelse (ETC$Item=="Project (160+ Hrs.)",99,	
	ifelse (ETC$Item=="Training",100,	
	ifelse (ETC$Item=="Data Migration/Update",101,	
	ifelse (ETC$Item=="SMTP allowance (add/del/modify)",102,	
	ifelse (ETC$Item=="Minor Enhancement (<40 Hrs.)','Minor Enhancement",103,	
	ifelse (ETC$Item=="Access level",104,	
	ifelse (ETC$Item=="Audio / Video",105,	
	ifelse (ETC$Item=="Circuit",106,	
	ifelse (ETC$Item=="Configuration change (non-Security)",107,	
	ifelse (ETC$Item=="Deploy",108,	
	ifelse (ETC$Item=="Desk",109,	
	ifelse (ETC$Item=="Enhancement (<40 Hrs.)",110,	
	ifelse (ETC$Item=="Forms/workflow",111,	
	ifelse (ETC$Item=="IPS allowance/change",112,	
	ifelse (ETC$Item=="Light",113,	
	ifelse (ETC$Item=="Major Enhancement ( 40-160 Hrs.)','Major Enhancement (40-160 Hrs.)",114,	
	ifelse (ETC$Item=="Record Retention - How To",115,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Creation",116,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Permission",117,	
	ifelse (ETC$Item=="Resource (add/del/modify)",118,	
	ifelse (ETC$Item=="Slow Performance",119,	
	ifelse (ETC$Item=="Standard",120,	
	ifelse (ETC$Item=="Switch",121,	
	ifelse (ETC$Item=="VPN Tunnel add (non-client)",122,	
	ifelse (ETC$Item=="Voicemail -Setup",123,	paste(ETC$Item_O1))))))))))))))))))))))))))))
	
ETC$Item_O1 <- ifelse (ETC$Item_O1 == "NA",86, paste(ETC$Item_O1))	
		
ETC$Item_O2 <- "NA"

ETC$Item_O2 <-	ifelse (ETC$Item=="Project (160+ Hrs.)",1,	
	ifelse (ETC$Item=="Chair",2,	
	ifelse (ETC$Item=="Site add/remove",3,	
	ifelse (ETC$Item=="Error Selection",4,	
	ifelse (ETC$Item=="Add Category/Sub-category/Item",5,	
	ifelse (ETC$Item=="Voicemail password reset",6,	
	ifelse (ETC$Item=="Wet floor",7,	
	ifelse (ETC$Item=="Firewall remove rule/setting",8,	
	ifelse (ETC$Item=="Checkout & Firefighter ID",9,	
	ifelse (ETC$Item=="Add/Remove Group",10,	
	ifelse (ETC$Item=="Lock Out / Password Reset','Lock Out/Password Reset",11,	
	ifelse (ETC$Item=="Not Assigned",12,	
	ifelse (ETC$Item=="Email address Setup",13,	
	ifelse (ETC$Item=="Update",14,	
	ifelse (ETC$Item=="Distribution List (add/del/modify)",15,	
	ifelse (ETC$Item=="SAP Logon pad replace",16,	
	ifelse (ETC$Item=="Disable",17,	
	ifelse (ETC$Item=="Install",18,	
	ifelse (ETC$Item=="Lock Out/Password Reset (Tier 1 for all but HR)",19,	
	ifelse (ETC$Item=="Firewall add rule/setting",20,	
	ifelse (ETC$Item=="Manager change/Cost center change",21,	
	ifelse (ETC$Item=="Remove",22,	
	ifelse (ETC$Item=="Firewall change rule/setting",23,	
	ifelse (ETC$Item=="Setup/Configuration",24,	
	ifelse (ETC$Item=="Add to Domain",25,	
	ifelse (ETC$Item=="Restore",26,	
	ifelse (ETC$Item=="How-To','How To",27,	
	ifelse (ETC$Item=="Remove Account",28,	
	ifelse (ETC$Item=="SAP GUI Instalation",29,	
	ifelse (ETC$Item=="Admin Access",30,	
	ifelse (ETC$Item=="Phone",31,	
	ifelse (ETC$Item=="Remove configuration",32,	
	ifelse (ETC$Item=="PC Enable",33,	
	ifelse (ETC$Item=="Spam",34,	
	ifelse (ETC$Item=="Modify",35,	
	ifelse (ETC$Item=="Call Forwarding - Disable",36,	
	ifelse (ETC$Item=="Grant special Permissions",37,	
	ifelse (ETC$Item=="Record Retention - Error",38,	
	ifelse (ETC$Item=="Password Reset",39,	
	ifelse (ETC$Item=="Voicemail - Remove",40,	
	ifelse (ETC$Item=="Decomission",41,	
	ifelse (ETC$Item=="Mailbox Full",42,	
	ifelse (ETC$Item=="Open Client Request",43,	
	ifelse (ETC$Item=="Physical Memory",44,	
	ifelse (ETC$Item=="Unblock",45,	
	ifelse (ETC$Item=="Error",46,	
	ifelse (ETC$Item=="Failure",47,	
	ifelse (ETC$Item=="Add",48,	paste(ETC$Item_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O2 <-	ifelse (ETC$Item=="Create Account",49,	
	ifelse (ETC$Item=="Block",50,	
	ifelse (ETC$Item=="New/Purchase",51,	
	ifelse (ETC$Item=="CPU LOAD",52,	
	ifelse (ETC$Item=="Install/Purchase",53,	
	ifelse (ETC$Item=="Call Forwarding - Enable",54,	
	ifelse (ETC$Item=="Modify configuration",55,	
	ifelse (ETC$Item=="Cubicle",56,	
	ifelse (ETC$Item=="Create Account/Object",57,	
	ifelse (ETC$Item=="Node Down",58,	
	ifelse (ETC$Item=="Configure/Setup",59,	
	ifelse (ETC$Item=="System clean/repair",60,	
	ifelse (ETC$Item=="Air freshener",61,	
	ifelse (ETC$Item=="Issue/Error",62,	
	ifelse (ETC$Item=="Request",63,	
	ifelse (ETC$Item=="Upgrade/Update",64,	
	ifelse (ETC$Item=="Replace",65,	
	ifelse (ETC$Item=="Access Request",66,	
	ifelse (ETC$Item=="Configuration",67,	
	ifelse (ETC$Item=="Loaner Request",68,	
	ifelse (ETC$Item=="Setup/Upgrade",69,	
	ifelse (ETC$Item=="Email address Change/Modify",70,	
	ifelse (ETC$Item=="Add/remove access",71,	
	ifelse (ETC$Item=="Change Account",72,	
	ifelse (ETC$Item=="Minor Enhancement (<40 Hrs.)",73,	
	ifelse (ETC$Item=="New/Replace",74,	
	ifelse (ETC$Item=="Access",75,	
	ifelse (ETC$Item=="Node Reboot",76,	
	ifelse (ETC$Item=="Break/Fix",77,	
	ifelse (ETC$Item=="Configuration change",78,	
	ifelse (ETC$Item=="Debug Request",79,	
	ifelse (ETC$Item=="User (add/del/modify)",80,	
	ifelse (ETC$Item=="Other",81,	
	ifelse (ETC$Item=="Volume Capacity",82,	
	ifelse (ETC$Item=="Disk Failure",83,	
	ifelse (ETC$Item=="New configuration",84,	
	ifelse (ETC$Item=="Maintenance",85,	
	ifelse (ETC$Item=="Role Assign",86,	
	ifelse (ETC$Item=="Information Request",87,	
	ifelse (ETC$Item=="Customer request (non-Security)",88,	
	ifelse (ETC$Item=="Voicemail - How To",89,	
	ifelse (ETC$Item=="Error/Failure",90,	
	ifelse (ETC$Item=="Upgrade",91,	
	ifelse (ETC$Item=="Add/Remove Technician",92,	
	ifelse (ETC$Item=="Role Change",93,	
	ifelse (ETC$Item=="Training",94,	
	ifelse (ETC$Item=="File restore",95,	
	ifelse (ETC$Item=="Printer Issue",96,	paste(ETC$Item_O2)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O2 <-	ifelse (ETC$Item=="Agent Deploy/Configure",97,	
	ifelse (ETC$Item=="Data Migration/Update",98,	
	ifelse (ETC$Item=="Restore from archive",99,	
	ifelse (ETC$Item=="Role Create",100,	
	ifelse (ETC$Item=="SMTP allowance (add/del/modify)",101,	
	ifelse (ETC$Item=="Configuration change (non-OS)",102,	
	ifelse (ETC$Item=="Provision/Remove Application",103,	paste(ETC$Item_O2))))))))

ETC$Item_O2 <- ifelse (ETC$Item_O2 == "NA",81, paste(ETC$Item_O2))	

ETC$Item_O3 <- "NA"
		
ETC$Item_O3 <-	ifelse (ETC$Item=="Remove configuration",1,	
	ifelse (ETC$Item=="Light",2,	
	ifelse (ETC$Item=="Password Reset",3,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Creation",4,	
	ifelse (ETC$Item=="SAP Logon pad replace",5,	
	ifelse (ETC$Item=="Voicemail -Setup",6,	
	ifelse (ETC$Item=="Call Forwarding - Disable",7,	
	ifelse (ETC$Item=="Standard",8,	
	ifelse (ETC$Item=="Install",9,	
	ifelse (ETC$Item=="Voicemail password reset",10,	
	ifelse (ETC$Item=="CPU LOAD",11,	
	ifelse (ETC$Item=="Configuration change (non-OS)",12,	
	ifelse (ETC$Item=="Remove Account",13,	
	ifelse (ETC$Item=="Cubicle",14,	
	ifelse (ETC$Item=="Site add/remove",15,	
	ifelse (ETC$Item=="Disable",16,	
	ifelse (ETC$Item=="Configuration",17,	
	ifelse (ETC$Item=="Record Retention - How To",18,	
	ifelse (ETC$Item=="Phone",19,	
	ifelse (ETC$Item=="Lock Out/Password Reset (Tier 1 for all but HR)",20,	
	ifelse (ETC$Item=="Physical Memory",21,	
	ifelse (ETC$Item=="SMTP allowance (add/del/modify)",22,	
	ifelse (ETC$Item=="SAP GUI Instalation",23,	
	ifelse (ETC$Item=="Restore",24,	
	ifelse (ETC$Item=="Access",25,	
	ifelse (ETC$Item=="Disk Failure",26,	
	ifelse (ETC$Item=="Circuit",27,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Permission",28,	
	ifelse (ETC$Item=="Remove",29,	
	ifelse (ETC$Item=="Admin Access",30,	
	ifelse (ETC$Item=="Manager change/Cost center change",31,	
	ifelse (ETC$Item=="Agent Deploy/Configure",32,	
	ifelse (ETC$Item=="Add/Remove Technician",33,	
	ifelse (ETC$Item=="Node Reboot",34,	
	ifelse (ETC$Item=="Data Migration/Update",35,	
	ifelse (ETC$Item=="Desk",36,	
	ifelse (ETC$Item=="Create Account",37,	
	ifelse (ETC$Item=="Mailbox Full",38,	
	ifelse (ETC$Item=="Create Account/Object",39,	
	ifelse (ETC$Item=="Role Assign",40,	
	ifelse (ETC$Item=="Switch",41,	
	ifelse (ETC$Item=="Grant special Permissions",42,	
	ifelse (ETC$Item=="Restore from archive",43,	
	ifelse (ETC$Item=="Access Request",44,	
	ifelse (ETC$Item=="Add to Domain",45,	
	ifelse (ETC$Item=="User (add/del/modify)",46,	
	ifelse (ETC$Item=="Request",47,	
	ifelse (ETC$Item=="Lock Out/Password Reset','Lock Out / Password Reset",48,	paste(ETC$Item_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O3 <-	ifelse (ETC$Item=="Modify",49,	
	ifelse (ETC$Item=="Add Category/Sub-category/Item",50,	
	ifelse (ETC$Item=="Block",51,	
	ifelse (ETC$Item=="Node Down",52,	
	ifelse (ETC$Item=="Add/remove access",53,	
	ifelse (ETC$Item=="Loaner Request",54,	
	ifelse (ETC$Item=="Record Retention - Error",55,	
	ifelse (ETC$Item=="Modify configuration",56,	
	ifelse (ETC$Item=="Training",57,	
	ifelse (ETC$Item=="PC Enable",58,	
	ifelse (ETC$Item=="File restore",59,	
	ifelse (ETC$Item=="New configuration",60,	
	ifelse (ETC$Item=="Error",61,	
	ifelse (ETC$Item=="Volume Capacity",62,	
	ifelse (ETC$Item=="Change Account",63,	
	ifelse (ETC$Item=="Failure",64,	
	ifelse (ETC$Item=="Setup/Configuration",65,	
	ifelse (ETC$Item=="Configure/Setup",66,	
	ifelse (ETC$Item=="Spam",67,	
	ifelse (ETC$Item=="Maintenance",68,	
	ifelse (ETC$Item=="Unblock",69,	
	ifelse (ETC$Item=="How To','How-To",70,	
	ifelse (ETC$Item=="Printer Issue",71,	
	ifelse (ETC$Item=="System clean/repair",72,	
	ifelse (ETC$Item=="Access level",73,	
	ifelse (ETC$Item=="Customer request (non-Security)",74,	
	ifelse (ETC$Item=="Deploy",75,	
	ifelse (ETC$Item=="Install/Purchase",76,	
	ifelse (ETC$Item=="Error/Failure",77,	
	ifelse (ETC$Item=="Information Request",78,	
	ifelse (ETC$Item=="Issue/Error",79,	
	ifelse (ETC$Item=="Distribution List (add/del/modify)",80,	
	ifelse (ETC$Item=="Configuration change",81,	
	ifelse (ETC$Item=="Break/Fix",82,	
	ifelse (ETC$Item=="Replace",83,	
	ifelse (ETC$Item=="Other",84,	
	ifelse (ETC$Item=="Firewall remove rule/setting",85,	
	ifelse (ETC$Item=="Update",86,	
	ifelse (ETC$Item=="Upgrade/Update",87,	
	ifelse (ETC$Item=="Role Change",88,	
	ifelse (ETC$Item=="Resource (add/del/modify)",89,	
	ifelse (ETC$Item=="VPN Tunnel add (non-client)",90,	
	ifelse (ETC$Item=="New/Replace",91,	
	ifelse (ETC$Item=="Configuration change (non-Security)",92,	
	ifelse (ETC$Item=="Minor Enhancement','Minor Enhancement (<40 Hrs.)",93,	
	ifelse (ETC$Item=="IPS allowance/change",94,	
	ifelse (ETC$Item=="Enhancement (<40 Hrs.)",95,	
	ifelse (ETC$Item=="Voicemail - How To",96,	paste(ETC$Item_O3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_O3 <-	ifelse (ETC$Item=="New/Purchase",97,	
	ifelse (ETC$Item=="Decomission",98,	
	ifelse (ETC$Item=="Role Create",99,	
	ifelse (ETC$Item=="Call Forwarding - Enable",100,	
	ifelse (ETC$Item=="Upgrade",101,	
	ifelse (ETC$Item=="Add/Remove Group",102,	
	ifelse (ETC$Item=="Setup/Upgrade",103,	
	ifelse (ETC$Item=="Firewall change rule/setting",104,	
	ifelse (ETC$Item=="Firewall add rule/setting",105,	
	ifelse (ETC$Item=="Email address Change/Modify",106,	
	ifelse (ETC$Item=="Forms/workflow",107,	
	ifelse (ETC$Item=="Project (160+ Hrs.)",108,	
	ifelse (ETC$Item=="Email address Setup",109,	
	ifelse (ETC$Item=="Major Enhancement (40-160 Hrs.)','Major Enhancement ( 40-160 Hrs.)",110,	
	ifelse (ETC$Item=="Audio / Video",111,	
	ifelse (ETC$Item=="Debug Request",112,	
	ifelse (ETC$Item=="Not Assigned",113,	
	ifelse (ETC$Item=="Slow Performance",114,	paste(ETC$Item_O3)))))))))))))))))))

ETC$Item_O3 <- ifelse (ETC$Item_O3 == "NA",84, paste(ETC$Item_O3))	

ETC$Item_R3 <- "NA"
		
ETC$Item_R3 <-	ifelse (ETC$Item=="Remove configuration",1,	
	ifelse (ETC$Item=="Light",1,	
	ifelse (ETC$Item=="Password Reset",1,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Creation",1,	
	ifelse (ETC$Item=="SAP Logon pad replace",1,	
	ifelse (ETC$Item=="Voicemail -Setup",1,	
	ifelse (ETC$Item=="Call Forwarding - Disable",1,	
	ifelse (ETC$Item=="Standard",1,	
	ifelse (ETC$Item=="Install",1,	
	ifelse (ETC$Item=="Voicemail password reset",1,	
	ifelse (ETC$Item=="CPU LOAD",1,	
	ifelse (ETC$Item=="Configuration change (non-OS)",1,	
	ifelse (ETC$Item=="Remove Account",1,	
	ifelse (ETC$Item=="Cubicle",1,	
	ifelse (ETC$Item=="Site add/remove",1,	
	ifelse (ETC$Item=="Disable",1,	
	ifelse (ETC$Item=="Configuration",1,	
	ifelse (ETC$Item=="Record Retention - How To",1,	
	ifelse (ETC$Item=="Phone",1,	
	ifelse (ETC$Item=="Lock Out/Password Reset (Tier 1 for all but HR)",1,	
	ifelse (ETC$Item=="Physical Memory",1,	
	ifelse (ETC$Item=="SMTP allowance (add/del/modify)",1,	
	ifelse (ETC$Item=="SAP GUI Instalation",1,	
	ifelse (ETC$Item=="Restore",1,	
	ifelse (ETC$Item=="Access",1,	
	ifelse (ETC$Item=="Disk Failure",1,	
	ifelse (ETC$Item=="Circuit",1,	
	ifelse (ETC$Item=="Record Retention Drive - Folder Permission",1,	
	ifelse (ETC$Item=="Remove",1,	
	ifelse (ETC$Item=="Admin Access",1,	
	ifelse (ETC$Item=="Manager change/Cost center change",1,	
	ifelse (ETC$Item=="Agent Deploy/Configure",1,	
	ifelse (ETC$Item=="Add/Remove Technician",1,	
	ifelse (ETC$Item=="Node Reboot",1,	
	ifelse (ETC$Item=="Data Migration/Update",1,	
	ifelse (ETC$Item=="Desk",1,	
	ifelse (ETC$Item=="Create Account",1,	
	ifelse (ETC$Item=="Mailbox Full",1,	
	ifelse (ETC$Item=="Create Account/Object",1,	
	ifelse (ETC$Item=="Role Assign",1,	
	ifelse (ETC$Item=="Switch",1,	
	ifelse (ETC$Item=="Grant special Permissions",1,	
	ifelse (ETC$Item=="Restore from archive",1,	
	ifelse (ETC$Item=="Access Request",2,	
	ifelse (ETC$Item=="Add to Domain",2,	
	ifelse (ETC$Item=="User (add/del/modify)",2,	
	ifelse (ETC$Item=="Request",2,	
	ifelse (ETC$Item=="Lock Out/Password Reset','Lock Out / Password Reset",2,	paste(ETC$Item_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_R3 <-	ifelse (ETC$Item=="Modify",2,	
	ifelse (ETC$Item=="Add Category/Sub-category/Item",2,	
	ifelse (ETC$Item=="Block",2,	
	ifelse (ETC$Item=="Node Down",2,	
	ifelse (ETC$Item=="Add/remove access",2,	
	ifelse (ETC$Item=="Loaner Request",2,	
	ifelse (ETC$Item=="Record Retention - Error",2,	
	ifelse (ETC$Item=="Modify configuration",2,	
	ifelse (ETC$Item=="Training",2,	
	ifelse (ETC$Item=="PC Enable",2,	
	ifelse (ETC$Item=="File restore",2,	
	ifelse (ETC$Item=="New configuration",2,	
	ifelse (ETC$Item=="Error",3,	
	ifelse (ETC$Item=="Volume Capacity",3,	
	ifelse (ETC$Item=="Change Account",3,	
	ifelse (ETC$Item=="Failure",3,	
	ifelse (ETC$Item=="Setup/Configuration",3,	
	ifelse (ETC$Item=="Configure/Setup",3,	
	ifelse (ETC$Item=="Spam",3,	
	ifelse (ETC$Item=="Maintenance",3,	
	ifelse (ETC$Item=="Unblock",3,	
	ifelse (ETC$Item=="How To','How-To",3,	
	ifelse (ETC$Item=="Printer Issue",3,	
	ifelse (ETC$Item=="System clean/repair",3,	
	ifelse (ETC$Item=="Access level",3,	
	ifelse (ETC$Item=="Customer request (non-Security)",4,	
	ifelse (ETC$Item=="Deploy",4,	
	ifelse (ETC$Item=="Install/Purchase",4,	
	ifelse (ETC$Item=="Error/Failure",4,	
	ifelse (ETC$Item=="Information Request",4,	
	ifelse (ETC$Item=="Issue/Error",4,	
	ifelse (ETC$Item=="Distribution List (add/del/modify)",4,	
	ifelse (ETC$Item=="Configuration change",4,	
	ifelse (ETC$Item=="Break/Fix",4,	
	ifelse (ETC$Item=="Replace",5,	
	ifelse (ETC$Item=="Other",5,	
	ifelse (ETC$Item=="Firewall remove rule/setting",5,	
	ifelse (ETC$Item=="Update",5,	
	ifelse (ETC$Item=="Upgrade/Update",5,	
	ifelse (ETC$Item=="Role Change",5,	
	ifelse (ETC$Item=="Resource (add/del/modify)",5,	
	ifelse (ETC$Item=="VPN Tunnel add (non-client)",5,	
	ifelse (ETC$Item=="New/Replace",5,	
	ifelse (ETC$Item=="Configuration change (non-Security)",6,	
	ifelse (ETC$Item=="Minor Enhancement','Minor Enhancement (<40 Hrs.)",6,	
	ifelse (ETC$Item=="IPS allowance/change",6,	
	ifelse (ETC$Item=="Enhancement (<40 Hrs.)",6,	
	ifelse (ETC$Item=="Voicemail - How To",6,	paste(ETC$Item_R3)))))))))))))))))))))))))))))))))))))))))))))))))
ETC$Item_R3 <-	ifelse (ETC$Item=="New/Purchase",6,	
	ifelse (ETC$Item=="Decomission",6,	
	ifelse (ETC$Item=="Role Create",6,	
	ifelse (ETC$Item=="Call Forwarding - Enable",6,	
	ifelse (ETC$Item=="Upgrade",6,	
	ifelse (ETC$Item=="Add/Remove Group",6,	
	ifelse (ETC$Item=="Setup/Upgrade",6,	
	ifelse (ETC$Item=="Firewall change rule/setting",6,	
	ifelse (ETC$Item=="Firewall add rule/setting",6,	
	ifelse (ETC$Item=="Email address Change/Modify",6,	
	ifelse (ETC$Item=="Forms/workflow",6,	
	ifelse (ETC$Item=="Project (160+ Hrs.)",6,	
	ifelse (ETC$Item=="Email address Setup",6,	
	ifelse (ETC$Item=="Major Enhancement (40-160 Hrs.)','Major Enhancement ( 40-160 Hrs.)",6,	
	ifelse (ETC$Item=="Audio / Video",6,	
	ifelse (ETC$Item=="Debug Request",6,	
	ifelse (ETC$Item=="Not Assigned",6,	
	ifelse (ETC$Item=="Slow Performance",6,	paste(ETC$Item_R3)))))))))))))))))))

ETC$Item_R3 <- ifelse (ETC$Item_R3 == "NA",5, paste(ETC$Item_R3))	


###################################################################################################


ETC$Created_Date       <- sapply(strsplit(as.character(ETC$CreatedTime), "\\ "), "[", 1)
ETC$Created_Time       <- sapply(strsplit(as.character(ETC$CreatedTime), "\\ "), "[", 2)

ETC$Created_month         <- sapply(strsplit(as.character(ETC$Created_Date), "\\/"), "[", 1)
ETC$Created_day          <- sapply(strsplit(as.character(ETC$Created_Date), "\\/"), "[", 2)
ETC$Created_year          <- sapply(strsplit(as.character(ETC$Created_Date), "\\/"), "[", 3)

ETC$Created_Hour          <- sapply(strsplit(as.character(ETC$Created_Time), "\\:"), "[", 1)
ETC$Created_Minutes       <- sapply(strsplit(as.character(ETC$Created_Time), "\\:"), "[", 2)

ETC$CreatedTime   <- strptime(ETC$CreatedTime,format=("%m/%d/%Y %H:%M"))
ETC$CompletedTime <- strptime(ETC$CompletedTime,format=("%m/%d/%Y %H:%M"))

ETC$Created_DayofWeek     <- weekdays(as.Date(ETC$CreatedTime))
ETC$Created_Month         <- months(as.Date(ETC$CreatedTime)) 

ETC$Duration      <- ETC$CompletedTime - ETC$CreatedTime 

ETC$Days_Taken    <- ifelse(ETC$Duration <= 86400,"less","more")


ETC$CreatedHour_R <-	ifelse (ETC$Created_Hour==2 | ETC$Created_Hour==3 | ETC$Created_Hour==4 | ETC$Created_Hour==5 | ETC$Created_Hour==8 | ETC$Created_Hour==9,2,
                      ifelse (ETC$Created_Hour==6 | ETC$Created_Hour==7,1,
                      ifelse (ETC$Created_Hour==10 | ETC$Created_Hour==11 | ETC$Created_Hour==19 | ETC$Created_Hour==20,3,
                      ifelse (ETC$Created_Hour==12 | ETC$Created_Hour==18,4,
                      ifelse (ETC$Created_Hour==0 | ETC$Created_Hour==1 | ETC$Created_Hour==13 | ETC$Created_Hour==14 | ETC$Created_Hour==16 | ETC$Created_Hour==17 | ETC$Created_Hour==21 | ETC$Created_Hour==22 | ETC$Created_Hour==23,5,6)))))
                                                     


ETC$CreatedDayofWeek_O1 <-	ifelse (ETC$Created_DayofWeek=="Saturday",1,
	ifelse (ETC$Created_DayofWeek=="Sunday",2,
	ifelse (ETC$Created_DayofWeek=="Monday",3,
	ifelse (ETC$Created_DayofWeek=="Wednesday",4,
	ifelse (ETC$Created_DayofWeek=="Tuesday",5,
	ifelse (ETC$Created_DayofWeek=="Thursday",6,
	ifelse (ETC$Created_DayofWeek=="Friday",7,
	5)))))))
	
ETC$CreatedDayofWeek_O2 <-	ifelse (ETC$Created_DayofWeek=="Friday",1,
	ifelse (ETC$Created_DayofWeek=="Saturday",2,
	ifelse (ETC$Created_DayofWeek=="Thursday",3,
	ifelse (ETC$Created_DayofWeek=="Wednesday",4,
	ifelse (ETC$Created_DayofWeek=="Sunday",5,
	ifelse (ETC$Created_DayofWeek=="Monday",6,
	ifelse (ETC$Created_DayofWeek=="Tuesday",7,
	5)))))))
	
ETC$CreatedDayofWeek_O3 <-	ifelse (ETC$Created_DayofWeek=="Saturday",1,
	ifelse (ETC$Created_DayofWeek=="Sunday",2,
	ifelse (ETC$Created_DayofWeek=="Tuesday",3,
	ifelse (ETC$Created_DayofWeek=="Monday",4,
	ifelse (ETC$Created_DayofWeek=="Friday",5,
	ifelse (ETC$Created_DayofWeek=="Wednesday",6,
	ifelse (ETC$Created_DayofWeek=="Thursday",7,
	5)))))))

##############

ETC <- na.omit(ETC)

###################################################################################################


## count for days ##

ETC$count_for_days<- 1

for (i in 2:5646)
{
ETC[i,942] <- ifelse(ETC[i,927] == ETC[i-1,927],ETC[i-1,942]+1,1) 
}

## count for created hour ##

ETC$Created_Date_Created_Hour <- do.call(paste, c(ETC[c("Created_Date","Created_Hour")],sep="-"))

ETC$count_for_hours <- 1

for (i in 2:5646)
{
  ETC[i,944] <- ifelse(ETC[i,943] == ETC[i-1,943],ETC[i-1,944]+1,1) 
}


setwd("D:/R Output files")
write.csv(ETC,file="output.csv")


## pivot based count w.r to Created hour ##

hour_based_count <- aggregate(ETC$CNT, by=list(ETC$Created_Hour), FUN = sum)



                                                                                                                                                                  

