{application, simreg,
 [
    {description, "SimReg is the MTNN SIM Registration SMS application module"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            txq, tx, rx, simreg_services,
            nanny, simreg, tx_sup, rx_sup,
            puk, reg, simreg_misultin, sms, simregctl,
            util
        ]
   },

  {registered, 
      [simreg, txq, tx_sup, rx_sup, nanny, simreg_misultin]
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

