
rm(list = ls()); gc()
library(RODBC)
library(ggplot2)
source('shikong_functions.R')

options(stringsAsFactors = F)
CRM = odbcConnect('crm_bak', uid = 'crmread', pwd='')

# 设定日期 部门
start_date ="2017-01-01"
end_date = "2017-01-31"
departname = "电话续约A部"
sqlStr = "select name, ep_ygbh, department, ReceiveDate, sum(CountMoney) as sumMoney 
          from (select distinct c.name, c.ep_ygbh, c.department, a.ReceiveDate, a.CountMoney from 
          Comm_pay a, Member b, chem99oa.dbo.admin c where a.MemberID = b.ID and b.Adminid = c.ID 
          and a.receivedate between 'start_date' and 'end_date' 
          and c.bumenid in (select child.id from chem99oa.dbo.admin_bumen as parent,
          chem99oa.dbo.admin_bumen as child where child.Lft >= parent.Lft
          and child.Rgt <= parent.Rgt and parent.bumen in ('departname')) and c.isusing =1) A 
          group by name, ep_ygbh, department, ReceiveDate order by ReceiveDate"
sqlStr = gsub('start_date', start_date, sqlStr)
sqlStr = gsub('end_date', end_date, sqlStr)
sqlStr = gsub('departname', departname, sqlStr)
print(sqlStr)

# 获取员工每天的业绩金额
input_data = sqlQuery(CRM, sqlStr)
colnames(input_data) = c('name', 'ep_ygbh','dept','date','value')
input_data$date = as.Date(input_data$date)
avg_data = aggregate(value ~ date, input_data, mean)
colnames(avg_data) = c('date', 'avg_value')

# 得出每天的业绩与平均业绩之差
dat = merge(input_data, avg_data, by = "date")
dat$diff = dat$value - dat$avg_value


# 连续失控 单边失控阈值
limit_num = 6

controlCeiling = 200
controlFloor =2
standardCeiling = 265
standardFloor = -62

result1= NULL
result2 = NULL
result3 = NULL
persons = unique(dat$ep_ygbh)

idnum = 1
res = NULL
res_all = NULL
for(i in 1:length(persons))
{
  print(paste(i, persons[i]))
  # 第i个人的数据
  dat0 = dat[dat$ep_ygbh == persons[i], ] 
  num = nrow(dat0) 

  if (num > 3) 
  {
    values = dat0$value
    diff_values = dat0$diff
    flag = rep(0, num) 
    flag[1] = 1

    # 对第i个人的数据分组    
    for(j in 2:num)     
    {
      tmp = diff_values[j] * diff_values[j-1]
      if(tmp > 0) flag[j] = flag[j-1]  
      if(tmp <= 0) flag[j] = flag[j-1] + 1
    }
    # dat02 = cbind(dat0, flag = flag)
    filter_flags = filterFlag(flag, limit_num)
    filter_index = which(flag %in% filter_flags)
    
    for(flag0 in filter_flags)
    {
      index1 = which(flag == flag0)
      dat01 = dat0[index1, ]
      start = min(dat01$date)
      end = max(dat01$date)
      len = nrow(dat01)
      res_cons = cbind(id = idnum, unique(dat01[, c('name', 'dept', 'ep_ygbh')]), 
                       start, end, len, type = '连续失控')
      res = rbind(res, res_cons)
      res_all = rbind(res_all, cbind(id = idnum, dat01))
            
      filename0 = paste(unique(dat01[, c('name', 'dept')]), collapse = '-')
      filename = paste('./continue_shikong/', idnum, '-', filename0, '.jpeg', sep = '')
      title = paste(filename0, '  ', start, '至', end, ' 共', len, '天', sep='')
      
      # 将失控点输出为图片
      plot_shikong(dat01, filename, title)
      idnum = idnum + 1
    }
  }
}
res = res[order(-res$len), ]
res = res[order(res$id), ]

write.csv(res, './continue_shikong/res.csv', row.names = F)
write.csv(res_all, './continue_shikong/res_all.csv', row.names = F)
