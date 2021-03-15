#!/bin/bash
mvn -s e:\\_emvn\\mset_j8_mv363_con_NC\\settings_sqdg_console.xml exec:exec -Dexec.executable="java" -Dexec.args="-classpath %classpath fun.gravax.buxapp.wsrv.RunGrvdatWsrvc"
