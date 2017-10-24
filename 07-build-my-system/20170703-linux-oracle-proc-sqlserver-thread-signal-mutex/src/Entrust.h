#ifndef ENTRUST_H
#define ENTRUST_H

#define ONE_SECOND 1000000

typedef struct CEntrustData{
    int l_report_serial_no;
    int l_entrust_serial_no;
    double en_entrust_price;
    int l_entrust_amount;
    double en_balance;
    char c_cancel_flag[2];
    char c_entrust_direction[6];
    char c_price_type[2];
    char c_entrust_status[2];
    char vc_stockholder_id[30];
    char vc_report_code[21];
    char vc_report_direction[3];
    char vc_report_seat[7];
    char vc_rowid[22];
    int l_fund_id;
    int l_basecombi_id;
    int c_stock_type[6];
    int l_cancel_rec_num;
    char vc_seat_id[7];
}EntrustData;

#endif
