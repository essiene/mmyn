-record(req, {datetime,seqnum,src,dst,msg}).
-record(res, {rt,src,dst,msg,status,op,code,detail,extra}).
-record(tlog, {tid, node, rxid, rxpid, smsc, port, system_id, handler, req, res}).
