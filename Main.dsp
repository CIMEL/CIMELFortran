# Microsoft Developer Studio Project File - Name="Main" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Main - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Main.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Main.mak" CFG="Main - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Main - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Main - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Main - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "Main - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /nologo /real_size:64 /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Main - Win32 Release"
# Name "Main - Win32 Debug"
# Begin Source File

SOURCE=.\alloc.f90
# End Source File
# Begin Source File

SOURCE=.\alloc1.f90
# End Source File
# Begin Source File

SOURCE=.\back_out_shura.f
DEP_F90_BACK_=\
	".\Debug\mo_par_DLS.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\BIBATM.f
# End Source File
# Begin Source File

SOURCE=.\BIBOD.f
# End Source File
# Begin Source File

SOURCE=.\COMMON_shura.f
# End Source File
# Begin Source File

SOURCE=.\fishmxSP_POL.f
# End Source File
# Begin Source File

SOURCE=.\intrpl_linear.f90
# End Source File
# Begin Source File

SOURCE=.\intrpl_spline.f90
# End Source File
# Begin Source File

SOURCE=.\iP2003LAST_shura.f
DEP_F90_IP200=\
	".\Debug\alloc.mod"\
	".\Debug\alloc1.mod"\
	".\Debug\mo_par_DLS.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\iterqP_N.f
# End Source File
# Begin Source File

SOURCE=.\Main.f
# End Source File
# Begin Source File

SOURCE=.\matrix_fixget_S.f
DEP_F90_MATRI=\
	".\Debug\alloc.mod"\
	".\Debug\alloc1.mod"\
	".\Debug\interpl_spline.mod"\
	".\Debug\intrpl_linear.mod"\
	".\Debug\mo_par_DLS.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\matrix_intrpl_LS.f
DEP_F90_MATRIX=\
	".\Debug\alloc.mod"\
	".\Debug\alloc1.mod"\
	".\Debug\interpl_spline.mod"\
	".\Debug\intrpl_linear.mod"\
	".\Debug\mo_par_DLS.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\matrix_optchr_LS.f
DEP_F90_MATRIX_=\
	".\Debug\intrpl_linear.mod"\
	".\Debug\mo_par_DLS.mod"\
	".\Debug\phase_func.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\mo_par_DLS.f90
# End Source File
# Begin Source File

SOURCE=.\phase_func.f90
DEP_F90_PHASE=\
	".\Debug\intrpl_linear.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sizedstr.f90
DEP_F90_SIZED=\
	".\Debug\mo_par_DLS.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SMOOTH.JUL.01.f
# End Source File
# End Target
# End Project
