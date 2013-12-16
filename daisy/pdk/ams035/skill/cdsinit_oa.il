/* 
File Name:    :    .cdsinit
File path:    :    $AMS_DIR/artist/start_up/cdsinit_template
Author        :    ams Design Support
Date          :    10-Dec-1996
Version:      :    1.0 (for analog artist) for Development HIT Kit
Include Files :
   $AMS_DIR/HK_ALL/skill/ams_env.il
   $AMS_DIR/HK_ALL/skill/ams_all.cxt
   $AMS_DIR/HK_C35/skill/ams_tips.il
   $AMS_DIR/HK_C35/skill/ams_sdl.il
   $AMS_DIR/HK_C35/skill/ams_c35b4.cxt

Owner: ams AG
HIT-Kit: Digital
*/
/*
 DESCRIPTION  

This is the template file for ams_cds

 HISTORY
--------------------------------------------------------------------------------
10.12.96  dpa  Created
17.03.97  dpa  Loading of standard cadence bindkeys dependant on existance
05.05.97  dpa  switch for 'interactive' tool added
19.06.97  dpa  P&R Bindkeys only loaded with icfb and icca
23.02.99  ghe  Add Internal Menu dependant on existance
01.06.99  tmo  Changed Version to "ams_3.70_sri_110523"
11.06.99  dpa  CDL callback renamed
20.07.99  dpa  Hard-coded paths replaced with AMS_LIB variable
21.01.00  tmo  Eldo load code added
11.04.00  tmo  Changed load strategy of callbacks
18.12.00  tmo  ATS load code added
22.02.01  tmo  rewrite load code for contexts
21.08.01  tmo  changed load order + added IDF interface
27.09.01  tmo  added load code for Agilent IFF link
13.12.02  hge  added load Bindkeys for "msfb"
10.01.03  tmo  added load code for HSIM
03.03.03  tmo  added load Bindkeys for "layoutPlus"
22.12.04  tmo  changed bindkey loader
16.06.06  tmo  added load code for Sandwork waveform viewer
30.10.06  tmo  added load code for Wicked
25.10.07  hge  added AMS_LASTMETAL and AMS_BEOL_STACK for h18
07.12.07  ghe  modifications for Wicked settings
26.08.09  tmo  changed load code for eldo
24.09.12  hge changed austriamicrosystems to ams
--------------------------------------------------------------------------------


*/

;###########################################################################
;#                                           
;# Global Variables
;#
;###########################################################################

AMS_TOP = getShellEnvVar("AMS_DIR") ; AMS_DIR
AMS_DB = "cds"
AMS_LIB = strcat(AMS_TOP "/" AMS_DB "/HK_C35") ; path to HIT-Kit Technology
AMS_VERSION = "ams_4.10" ; For ams_cds. Do NOT use for other purposes
AMS_TECH = "c35b4"
AMS_kit = "ams_4.10"
AMS_process = "c35b4"
AMS_tool = "artist"
CDS_QUIET = getShellEnvVar("CDS_QUIET")

if(getShellEnvVar("LASTMETAL")  then AMS_LASTMETAL = getShellEnvVar("LASTMETAL"))
if(getShellEnvVar("BEOL_STACK") then AMS_BEOL_STACK = strcat("M" getShellEnvVar("BEOL_STACK")))

;###########################################################################
;#                                           
;# Copyright Notice
;#
;###########################################################################
printf("......................................................................................\n")
printf("           ams AG HIT-Kit v.%s\n" AMS_kit)
printf("......................................................................................\n")
printf("COPYRIGHT (c) 2012 Full Service Foundry @ ams AG.  ALL RIGHTS RESERVED\n")
printf("This HIT-Kit and attached documentation are confidential information and may \n")
printf("only be used as authorized by the HIT-Kit license agreement.\n")
printf("......................................................................................\n")

;###########################################################################
;#                                           
;# Initialize CIW (needed for versions newer than 4.4.3)
;#
;###########################################################################
ciwMenuInit()

;###########################################################################
;#
;# LOAD REQUIRED SKILL FILES AND ADD THE HIT_KIT MENU
;#
;###########################################################################


if( !fboundp( 'amsInitContext ) then
  if( !CDS_QUIET then printf("Loading generic HIT-Kit skill context.\n"))

  cdsVersion=nth(1 parseString(getVersion(t)))

  case( AMS_DB
   ("artist"
    if(substring(cdsVersion 1 1)=="4" then
      case( substring(cdsVersion 1 5)
        ("4.4.5"
          loadContext(strcat(AMS_TOP "/artist/HK_ALL/skill/ams_all_445.cxt"))
        )
        ("4.4.6"
          loadContext(strcat(AMS_TOP "/artist/HK_ALL/skill/ams_all_446.cxt"))
        )
      )
    else
      case( substring(cdsVersion 1 3)
        (("5.0" "5.1")
          loadContext(strcat(AMS_TOP "/artist/HK_ALL/skill/ams_all_500.cxt"))
        )
      )
    )
   )
   ("cds"
     case( substring( cdsVersion 3 3 )
       (("OA5")
         loadContext(strcat(AMS_TOP "/cds/HK_ALL/skill/ams_all_52.cxt"))
       )
       (("6.1")
         loadContext(strcat(AMS_TOP "/cds/HK_ALL/skill/ams_all_61.cxt"))
       )   )
   )
  )
  callInitProc("ams_all")
  AMS_DIR = getShellEnvVar("AMS_DIR")
)

hiInsertBannerMenu(hiGetCIWindow() menuIdPull 9)

;###########################################################################
;#                                           
;# Set Default options fot CDLOut
;#
;###########################################################################

; Set the defaults for CdlOut
hiSetMenuItemCallback(ciwFileMenu->ExportItem->hiSubMenu ciwFileMenu->ExportItem
->hiSubMenu->CDLOut->hiMenuItemSym "amsCDLoutItemCallback()")

;###########################################################################
;#                                           
;# Set Default options 
;#
;###########################################################################

if( !CDS_QUIET then printf( "Loading the environment file.\n" ))
load(strcat(AMS_TOP "/" AMS_DB "/HK_ALL/skill/ams_env.il"))


;###########################################################################
;#                                           
;# load AMS specific callbacks 
;#
;###########################################################################

if( !CDS_QUIET then printf( "Loading process specific call-back routines.\n" ))

load(strcat(AMS_LIB "/skill/tips_"   lowerCase(AMS_process) ".il"))

if( isFile(strcat(AMS_LIB "/skill/ams_callBacks.il")) then
   load(strcat(AMS_LIB "/skill/ams_callBacks.il"))
else
   load(strcat(AMS_TOP "/" AMS_DB "/HK_ALL/skill/ams_callBacks.il"))
)

;###########################################################################
;#                                           
;# load AMS specific pCells
;#
;###########################################################################

if( !CDS_QUIET then printf( "Loading all available p-cell functions.\n" ))
load(strcat(AMS_LIB "/skill/ams_sdl.il"))

;###########################################################################
;#                                           
;# LOAD STANDARD BINDKEYS 
;#
;###########################################################################

procedure(loadif(file)
  if(isFile(file) then loadi(file))
)

if( !CDS_QUIET then printf( "Loading default bindkeys.\n" ))
let((bindKeyFile)
  if( (rexMatchp("icca" getVersion()) || rexMatchp("icde" getVersion()) || rexMatchp("icds" getVersion())  || rexMatchp("icfb" getVersion())  || rexMatchp("icms" getVersion()) || rexMatchp("msfb" getVersion()) || rexMatchp("layout" getVersion()) || rexMatchp("rfde" getVersion()) || rexMatchp("virtuoso" getVersion())  ) then
      loadif(strcat(prependInstallPath("samples/local/") "leBindKeys.il"))
      loadif(strcat(prependInstallPath("samples/local/") "lxBindKeys.il"))
      loadif(strcat(prependInstallPath("samples/local/") "schBindKeys.il"))
      ;loadif(strcat(prependInstallPath("samples/local/") "awvBindKeys.il"))
  )
  if( (rexMatchp("icca" getVersion()) || rexMatchp("icfb" getVersion()) ) then
      loadif(strcat(prependInstallPath("samples/local/") "prBindKeys.il"))
  )
) ;let


;###########################################################################
;#                                           
;# ELDO Interface 
;#
;###########################################################################

let((t_dir t_file)
    t_dir = getShellEnvVar("MGC_AMS_HOME")
    when(t_dir t_file = strcat(t_dir "/etc/cds/.cdsinit"))
    when(t_file && isFile(t_file) 
      if( !CDS_QUIET then printf("Loading ELDO Interface.\n"))
      load(t_file)
    )
)

;###########################################################################
;#                                           
;# HSPICE Interface 
;#
;###########################################################################

let((t_dir t_file)
    t_dir = getShellEnvVar("HSP_HOME")
    when(t_dir t_file = prependInstallPath( "local/HSPICE/HSPICE.ini" ))

    when(t_file && isFile(t_file) 
      if( !CDS_QUIET then printf("Loading HSPICE Interface.\n"))
      load(t_file)
    )
)

;###########################################################################
;#                                           
;# Dynamic Link Interface 
;#
;###########################################################################

let((idf_dir idf_file)
    when( getShellEnvVar("HPEESOF_DIR")
       idf_dir = strcat(getShellEnvVar("HPEESOF_DIR") "/idf")
       when(idf_dir idf_file = strcat(idf_dir "/config/.cdsinit"))
       when(idf_file && isFile(idf_file) 
          if( !CDS_QUIET then printf("Loading ADS Dynamic Link.\n"))
          load(idf_file)
          load(strcat(AMS_TOP "/" AMS_DB "/HK_ALL/skill/ams_idfnlfuncs.il"))
       )
    )
)

;###########################################################################
;#                                           
;# Add IFF Link 
;#
;###########################################################################

let((iff_dir iff_file)
    when( getShellEnvVar("HPEESOF_DIR")
       iff_dir = strcat(getShellEnvVar("HPEESOF_DIR") "/links/tools/iff/cadence/composer")
       when(iff_dir 
          iff_file = strcat(iff_dir "/mdsinit.il")
       )
       when(iff_file && isFile(iff_file) 
          if( !CDS_QUIET then println("Loading ADS IFF transfer link.") )
          load(iff_file)
          hiInsertBannerMenu(hiGetCIWindow() ciwAgilentIffLinkMenu 9)
       )
    )
)

;###########################################################################
;#                                           
;# Add GoldenGate 
;#
;###########################################################################

let((gg_dir gg_file)
    when( getShellEnvVar("XPEDION")
       gg_dir = strcat(getShellEnvVar("XPEDION") "/aa")
       when(gg_dir 
          gg_file = strcat(gg_dir "/.cdsinit")
       )
       when(gg_file && isFile(gg_file) 
          if( !CDS_QUIET then println("Loading Agilent GoldenGate.") )
          load(gg_file)
	  if( isFile(strcat(AMS_TOP "/" AMS_DB "/HK_ALL/skill/ams_GoldenGate.ile")) then
	    load( strcat(AMS_TOP "/" AMS_DB "/HK_ALL/skill/ams_GoldenGate.ile") "hitkit" )
	  )
       )
    )
)

;###########################################################################
;#                                           
;# HSIM Interface 
;#
;###########################################################################

let((hsim_file)
    when( getShellEnvVar("NASSDA_ARTISTIF")
       hsim_file=strcat( getShellEnvVar("NASSDA_ARTISTIF") "/hsimArtistIFInit.il")
       when( isFile(hsim_file)
          load( hsim_file );
       )
    )
)

;###########################################################################
;#                                           
;# NeoCircuit Interface 
;#
;###########################################################################

let((t_dir t_file)
    t_dir = getShellEnvVar("NEOCKTHOME")
    when(t_dir t_file = strcat(t_dir "/bin/NeoCircuit_cdsinit.il"))
    when(t_file && isFile(t_file) 
      if( !CDS_QUIET then printf("Loading NeoCircuit Interface.\n"))
      load(t_file)
    )
)

;###########################################################################
;#                                           
;# Wicked Interface 
;#
;###########################################################################

let((t_dir t_file wickedversion)
    setShellEnvVar("RUN_OCEAN=1")
    setShellEnvVar(strcat( "WICKED_ICFB_RUN_DIR=" getWorkingDir() )  )
    t_dir = getShellEnvVar("WICKED_INSTDIR")
    when(t_dir  setShellEnvVar(strcat("WICKED_INSTALLATION_DIR=" t_dir ) ) )
    when(t_dir t_file = strcat(t_dir "/config/WiCkeD.il"))
    when(t_file && isFile(t_file)
      if( !CDS_QUIET then printf("Loading WiCkeD Interface.\n"))
      load(t_file)
    )
    if(!(wickedversion=getShellEnvVar("WICKED_VERSION")) then wickedversion="")
    t_file=strcat( AMS_TOP "/muneda/site" wickedversion "/cdsinit_wicked" )
    when( isFile( t_file )
      if( !CDS_QUIET then printf("Loading WiCkeD Site Customization.\n"))
      load(t_file)
    )
    t_file=strcat( AMS_TOP "/muneda/flow" wickedversion "/ams_callback_setup.il" )
    when( isFile( t_file )
      if( !CDS_QUIET then printf("Setting Variable WICKED_EXT_SKILL.\n"))
      setShellEnvVar(strcat("WICKED_EXT_SKILL=" t_file ) )
    )
)


;###########################################################################
;#                                           
;# Sandwork Interface 
;#
;###########################################################################

let((t_dir t_dir2 t_file t_file2)
    t_dir = getShellEnvVar("SANDWORK_DIR")
    if( t_dir && isDir( strcat(t_dir "/packages")) then t_dir=strcat(t_dir "/packages") )
    when(t_dir t_file = strcat(t_dir "/SX-CDS-Link/sx_link.ile"))
    when(t_dir t_file2 = strcat(t_dir "/SX-CDS-Link/sx_menu.il"))
    when(t_file && isFile(t_file) 
      if( !CDS_QUIET then printf("Loading Sandwork Interface.\n"))
      load(t_file "sandwork")
      load(t_file2)
      if( fboundp('dbGetDatabaseType) then
        if( dbGetDatabaseType()=="OpenAccess" then
          envSetVal( "spectre.outputs" "simOutputFormat" 'string "psf")
        else
          envSetVal( "spectre.envOpts" "simOutputFormat" 'string "psfbin")
        )
      else
        envSetVal( "spectre.envOpts" "simOutputFormat" 'string "psfbin")
      )
      printf(" Setting Spectre Output format to psfbin.\n")

      if( envGetVal( "spectreVerilog.envOpts" "logicOutputFormat") != "WSF" then
        envSetVal( "spectreVerilog.envOpts" "logicOutputFormat" 'string "WSF")
        printf(" Setting SpectreVerilog Output format to WSF.\n")
      )
    )
)

;###########################################################################
;#                                           
;# Hercules Interface 
;#
;###########################################################################

let((t_dir s_type t_file)
    t_dir = getShellEnvVar("HERCULES_HOME_DIR")
    s_type = getShellEnvVar("SYNOPSYS_SYSTYPE")
    when(t_dir t_file = strcat(t_dir "/bin/" s_type "/skill_menu.il"))
    when(t_file && isFile(t_file) 
      if( !CDS_QUIET then printf("Loading Hercules Interface.\n"))
      load(t_file)
    )
)


;###########################################################################
;#                                           
;# SET THE PROMPT 
;#
;###########################################################################

if( !CDS_QUIET then printf( "Setting the prompt.\n" ))
AMS_setPrompt()

;###########################################################################
;#                                           
;# Call skill for the netlistingMode when tool == interactive
;#
;###########################################################################

when(AMS_tool == "interactive"  thamNetMode() ) 


;###########################################################################
;#                                           
;# ADD INTERNAL MENU 
;#
;###########################################################################

if(isFile("/user/libmgr/internal/skill/internalMenu.il") then
   if( !CDS_QUIET then printf("Loading INTERN menu.\n"))
   load("/user/libmgr/internal/skill/internalMenu.il")
)

;
;###########################################################################
;#                                           
;# LOAD USER CUSTOMIZATION FILE -- We already have our own facility for this
;#
;###########################################################################
/* added this to let users add their own customisation, without */
/* hacking the library .cdsinit file. CT, 04/10/95 */
/*
if( isFile( "./.cdsinit_local" ) then
    if( !CDS_QUIET then printf( "Loading ./.cdsinit_local file from .cdsinit file.\n" ))
    loadi( "./.cdsinit_local" )
  )
if( isFile( "~/.cdsinit_personal" ) then
   if( !CDS_QUIET then printf( "Loading $HOME/.cdsinit_personal file from .cdsinit file.\n" ))
   loadi( "~/.cdsinit_personal" )
  )
*/
;
;###########################################################################
;#                                           
;# Check Process Option
;#
;###########################################################################

AMS_checkProcessOption()


;###########################################################################
;#                                           
;# THE END! 
;#
;###########################################################################

if( !CDS_QUIET then printf("END OF SITE CUSTOMIZATION.\n"))
;END OF THE SITE CUSTOMIZATION FILE

