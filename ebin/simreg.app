{application, simreg,
 [
    {description, "SimReg is the MTNN SIM Registration SMS application module"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            backoff,
            txq, tx_sup, tx_nanny, esmetx, 
            rx_sup, rx_nanny, esmerx, 
            simreg_misultin, simreg,
            puk, reg, sms, simreg_services,
            simregctl, util
        ]
   },

  {registered, 
      [backoff, txq, tx_sup, tx_nanny, rx_sup, rx_nanny, simreg_misultin, simreg]
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

