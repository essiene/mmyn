{application, simreg,
 [
    {description, "SimReg is the MTNN SIM Registration SMS application module"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            backoff,
            txq, tx_sup, nanny, esmetx, 
            rx_sup, esmerx, 
            simreg_misultin, simreg,
            reg, sms, simreg_services,
            simregctl, util, gen_sms_handler,
            tlog
        ]
   },

  {registered, 
      [backoff, txq, tx_sup, tx_nanny, rx_sup, rx_nanny, simreg_misultin, simreg, tlog]
  },

  {applications, 
      [kernel, stdlib, sasl, ibrowse, log4erl]
  },

  {env, []},

  {mod, 
      {simreg, []}
  }
 ]
}.

