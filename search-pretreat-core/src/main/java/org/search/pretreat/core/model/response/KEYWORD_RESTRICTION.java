package org.search.pretreat.core.model.response;

public enum KEYWORD_RESTRICTION {
    NORMAL("0"),                             // 同时搜索美食名和餐厅名
    NORMAL_SKIP_SHOP_NAME_BRACKET("1"),      // 同时搜索美食名和餐厅名，但不匹配餐厅名括号里内容
    SHOP_NAME_ONLY("2"),                     // 只搜索餐厅名
    SHOP_NAME_ONLY_SKIP_BRACKET("3"),        // 只搜索餐厅名，不匹配餐厅名括号里内容
    FOOD_NAME_ONLY("4"),                     // 只搜索美食名
    PINYIN("5"),                             // 只限原词，默认只搜餐厅名并且不搜括号内容
    ALPHABETIC("6"),                         // 如果原词就没有汉字
    ALPHABETIC_SHOP_NAME_ONLY("7");          // 如果原词就没有汉字，只搜索餐厅名

    public String value;
    KEYWORD_RESTRICTION(String value) {
        this.value = value;
    }
}
