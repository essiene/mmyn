{application, simreg,
 [
    {description, "SimReg is the MTNN SIM Registration SMS application module"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            txq, simreg_tx, simreg_rx, simreg_services,
            nanny, simreg, simreg_rx_sup,
            puk, reg, simreg_misultin, webservice,
            sms, simregctl, util
        ]
   },

  {registered, 
      [simreg, txq, simreg_tx, simreg_rx_sup, nanny, simreg_misultin]
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

