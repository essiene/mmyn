{application, simreg,
 [
    {description, "SimReg is the MTNN SIM Registration SMS application module"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            simreg_tx, simreg_rx, simreg_services,
            simreg_manager, simreg, simreg_rx_sup,
            puk, regstatus, simreg_misultin, webservice,
            sms, simregctl, util
        ]
   },

  {registered, 
      [simreg_manager, simreg, simreg_tx, simreg_rx_sup, simreg_misultin]
  },

  {applications, 
      [kernel, stdlib, sasl, ibrowse]
  },

  {env, []},

  {mod, 
      {simreg, []}
  }
 ]
}.

