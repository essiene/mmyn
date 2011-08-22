@echo off
REM   The contents of this file are subject to the Mozilla Public License
REM   Version 1.1 (the "License"); you may not use this file except in
REM   compliance with the License. You may obtain a copy of the License at
REM   http://www.mozilla.org/MPL/
REM
REM   Software distributed under the License is distributed on an "AS IS"
REM   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
REM   License for the specific language governing rights and limitations
REM   under the License.
REM
REM   The Original Code is RabbitMQ.
REM
REM   The Initial Developers of the Original Code are LShift Ltd,
REM   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
REM
REM   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
REM   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
REM   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
REM   Technologies LLC, and Rabbit Technologies Ltd.
REM
REM   Portions created by LShift Ltd are Copyright (C) 2007-2010 LShift
REM   Ltd. Portions created by Cohesive Financial Technologies LLC are
REM   Copyright (C) 2007-2010 Cohesive Financial Technologies
REM   LLC. Portions created by Rabbit Technologies Ltd are Copyright
REM   (C) 2007-2010 Rabbit Technologies Ltd.
REM
REM   All Rights Reserved.
REM
REM   Contributor(s): ______________________________________.
REM

setlocal

rem Preserve values that might contain exclamation marks before
rem enabling delayed expansion
set TN0=%~n0
set TDP0=%~dp0
set P1=%1
set STAR=%*
setlocal enabledelayedexpansion

if "!MMAYEN_SERVICENAME!"=="" (
    set MMAYEN_SERVICENAME=Mmayen
)

if "!MMAYEN_RELEASE_PATH!"=="" (
    set MMAYEN_RELEASE_PATH=!MMAYEN_BASE!\releases\1
    echo "Release Path = !MMAYEN_RELEASE_PATH!"
    
)

if "!COMPUTERNAME!"=="" (
    set COMPUTERNAME=localhost
)

if "!MMAYEN_NODENAME!"=="" (
    set MMAYEN_NODENAME=mmyn@!COMPUTERNAME!
)

if "!MMAYEN_ERLANG_HOME!"=="" (
    set MMAYEN_ERLANG_HOME=!MMAYEN_BASE!\erts-5.8.2
)

if "!ERLANG_SERVICE_MANAGER_PATH!"=="" (
    if not exist "!MMAYEN_ERLANG_HOME!\bin\erl.exe" (
        echo.
        echo ******************************
        echo MMAYEN_BASE is not set correctly.
        echo ******************************
        echo.
        echo Please either set MMAYEN_BASE to point to you MMAYEN installation base
        echo.
        exit /B
    )

    set ERLANG_SERVICE_MANAGER_PATH=!MMAYEN_ERLANG_HOME!\bin
)

set CONSOLE_FLAG=
set CONSOLE_LOG_VALID=
for %%i in (new reuse) do if "%%i" == "!MMAYEN_CONSOLE_LOG!" set CONSOLE_LOG_VALID=TRUE
if "!CONSOLE_LOG_VALID!" == "TRUE" (
    set CONSOLE_FLAG=-debugtype !MMAYEN_CONSOLE_LOG!
)

rem *** End of configuration ***

if not exist "!ERLANG_SERVICE_MANAGER_PATH!\erlsrv.exe" (
    echo.
    echo **********************************************
    echo ERLANG_SERVICE_MANAGER_PATH not set correctly. 
    echo **********************************************
    echo.
    echo "!ERLANG_SERVICE_MANAGER_PATH!\erlsrv.exe" not found
    echo Please set ERLANG_SERVICE_MANAGER_PATH to the folder containing "erlsrv.exe".
    echo.
    exit /B 1
)

rem erlang prefers forwardslash as separator in paths
set MMAYEN_BASE_UNIX=!MMAYEN_BASE:\=/!

if "!MMAYEN_LOG_BASE!"=="" (
    set MMAYEN_LOG_BASE=!MMAYEN_BASE_UNIX!/log
)


rem We save the previous logs in their respective backup
rem Log management (rotation, filtering based on size...) is left as an exercise for the user.

set BACKUP_EXTENSION=.1

set LOGS=!MMAYEN_BASE!\log\!MMAYEN_NODENAME!.log
set SASL_LOGS=!MMAYEN_BASE!\log\!MMAYEN_NODENAME!-sasl.log

set LOGS_BACKUP=!MMAYEN_BASE!\log\!MMAYEN_NODENAME!.log!BACKUP_EXTENSION!
set SASL_LOGS_BACKUP=!MMAYEN_BASE!\log\!MMAYEN_NODENAME!-sasl.log!BACKUP_EXTENSION!

if exist "!LOGS!" (
	type "!LOGS!" >> "!LOGS_BACKUP!"
)
if exist "!SASL_LOGS!" (
	type "!SASL_LOGS!" >> "!SASL_LOGS_BACKUP!"
)

rem End of log management


if "!MMAYEN_PLUGINS_EXPAND_DIR!"=="" (
    set MMAYEN_PLUGINS_EXPAND_DIR=!MMAYEN_MNESIA_BASE!/!MMAYEN_NODENAME!-plugins-expand
)

if "!P1!" == "install" goto INSTALL_SERVICE
for %%i in (start stop disable enable list remove) do if "%%i" == "!P1!" goto MODIFY_SERVICE 

echo.
echo *********************
echo Service control usage
echo *********************
echo.
echo !TN0! help    - Display this help
echo !TN0! install - Install the !MMAYEN_SERVICENAME! service
echo !TN0! remove  - Remove the !MMAYEN_SERVICENAME! service
echo.
echo The following actions can also be accomplished by using 
echo Windows Services Management Console (services.msc):
echo.
echo !TN0! start   - Start the !MMAYEN_SERVICENAME! service
echo !TN0! stop    - Stop the !MMAYEN_SERVICENAME! service
echo !TN0! disable - Disable the !MMAYEN_SERVICENAME! service
echo !TN0! enable  - Enable the !MMAYEN_SERVICENAME! service
echo.
exit /B


:INSTALL_SERVICE

if not exist "!MMAYEN_BASE!" (
    echo Creating base directory !MMAYEN_BASE! & md "!MMAYEN_BASE!" 
)

set MMAYEN_BOOT_FILE="!MMAYEN_RELEASE_PATH!\mmyn"
if not exist "!MMAYEN_BOOT_FILE!.boot" (
    echo Custom Boot File "!MMAYEN_BOOT_FILE!.boot" is missing.
    exit /B 1
)
echo "BOOT FILE = !MMAYEN_BOOT_FILE!.boot"

if "!MMAYEN_CONFIG_FILE!"=="" (
    set MMAYEN_CONFIG_FILE=!MMAYEN_BASE!\etc\app
)
echo "CONFIG FILE = !MMAYEN_CONFIG_FILE!"
   
if exist "!MMAYEN_CONFIG_FILE!.config" (
    set MMAYEN_CONFIG_ARG=-config "!MMAYEN_CONFIG_FILE!"
) else (
    set MMAYEN_CONFIG_ARG=
)

if "!MMAYEN_VM_ARGS_FILE!"=="" (
    set MMAYEN_VM_ARGS_FILE=!MMAYEN_BASE!\etc\vm.args
)

echo "VM ARGS FILE = !MMAYEN_VM_ARGS_FILE!"

if exist "!MMAYEN_VM_ARGS!" (
    set MMAYEN_VM_ARG=-args_file "!MMAYEN_VM_ARGS_FILE!"
) else (
    set MMAYEN_VM_ARG=
)


"!ERLANG_SERVICE_MANAGER_PATH!\erlsrv" list !MMAYEN_SERVICENAME! 2>NUL 1>NUL
if errorlevel 1 (
    "!ERLANG_SERVICE_MANAGER_PATH!\erlsrv" add !MMAYEN_SERVICENAME!
) else (
    echo !MMAYEN_SERVICENAME! service is already present - only updating service parameters
)

set MMAYEN_EBIN_PATH=


set ERLANG_SERVICE_ARGUMENTS= ^
!MMAYEN_EBIN_PATH! ^
-boot "!MMAYEN_BOOT_FILE!" ^
!MMAYEN_CONFIG_ARG! ^
!MMAYEN_VM_ARG! ^
-s mmyn ^
+W w ^
+A30 ^
-kernel error_logger {file,\""!MMAYEN_LOG_BASE!/!MMAYEN_NODENAME!.log"\"} ^
!MMAYEN_SERVER_ERL_ARGS! ^
-sasl errlog_type error ^
-sasl sasl_error_logger {file,\""!MMAYEN_LOG_BASE!/!MMAYEN_NODENAME!-sasl.log"\"} ^
!MMAYEN_SERVER_START_ARGS! ^
!STAR!

set ERLANG_SERVICE_ARGUMENTS=!ERLANG_SERVICE_ARGUMENTS:\=\\!
set ERLANG_SERVICE_ARGUMENTS=!ERLANG_SERVICE_ARGUMENTS:"=\"!

"!ERLANG_SERVICE_MANAGER_PATH!\erlsrv" set !MMAYEN_SERVICENAME! ^
-machine "!ERLANG_SERVICE_MANAGER_PATH!\erl.exe" ^
-env ERL_CRASH_DUMP="!MMAYEN_BASE_UNIX!/erl_crash.dump" ^
-workdir "!MMAYEN_BASE!" ^
-stopaction "mmyn:stop_and_halt()." ^
-sname !MMAYEN_NODENAME! ^
!CONSOLE_FLAG! ^
-args "!ERLANG_SERVICE_ARGUMENTS!" > NUL

goto END


:MODIFY_SERVICE

"!ERLANG_SERVICE_MANAGER_PATH!\erlsrv" !P1! !MMAYEN_SERVICENAME!
goto END


:END

endlocal
endlocal
