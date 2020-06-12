package org.search.pretreat.core.model.response;

public enum INSTRUCTION_TYPE {
    ORIGIN_KEYWORD,     // 包括原词、同义词、重写词
    DROP_KEYWORD,
    DROP_KEYWORD_NEW,   // 算法挖掘的丢词，且能召回老丢词无法召回的东西
    CHANGE_KEYWORD,
    COVER,              // 只用来做美食补位
    FOOD_LABEL,         // 包括大数据标签和人工挖掘的标签
    FOOD_CATE,          // 包括餐饮和新零售的类目
    FOOD_LABEL_IDV,     // 算法挖掘的单字命中标签
    FOOD_LABEL_MUL,     // 算法挖掘的多字命中标签
    SHOP_LABEL1         // 包括label1和补充的人工挖掘标签
}
