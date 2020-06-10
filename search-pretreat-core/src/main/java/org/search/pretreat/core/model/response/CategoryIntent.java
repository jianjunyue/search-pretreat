package org.search.pretreat.core.model.response;


import java.util.Map;
 
public class CategoryIntent {
    // 店铺二级类目意图：key是类目id，value是分数
    private Map<Integer, Double> shopCategoryInfo;
    // 是否是新零售意图（key为"__label__1"和"__label__0"）
    private Map<String, Double> isRetailInfo;
    // 餐饮商品一级类目意图：key是类目名称（去掉所有符号只保留汉字），value是分数（当搜索词完全没有餐饮意图，为空）
    private Map<String, Double> foodCategoryInfo;
    // 餐饮商品二级类目意图：key是类目名称（去掉所有符号只保留汉字），value是分数（当搜索词完全没有餐饮意图，为空）
    private Map<String, Double> foodSubCategoryInfo;
    // 零售商品一级类目意图：key是类目名称（去掉所有符号只保留汉字），value是分数（当搜索词完全没有零售意图，为空）
    private Map<String, Double> retailCategoryInfo;

    public Map<Integer, Double> getShopCategoryInfo() {
        return shopCategoryInfo;
    }

    public void setShopCategoryInfo(Map<Integer, Double> shopCategoryInfo) {
        this.shopCategoryInfo = shopCategoryInfo;
    }

    public Map<String, Double> getIsRetailInfo() {
        return isRetailInfo;
    }

    public void setIsRetailInfo(Map<String, Double> isRetailInfo) {
        this.isRetailInfo = isRetailInfo;
    }

    public Map<String, Double> getFoodCategoryInfo() {
        return foodCategoryInfo;
    }

    public void setFoodCategoryInfo(Map<String, Double> foodCategoryInfo) {
        this.foodCategoryInfo = foodCategoryInfo;
    }

    public Map<String, Double> getFoodSubCategoryInfo() {
        return foodSubCategoryInfo;
    }

    public void setFoodSubCategoryInfo(Map<String, Double> foodSubCategoryInfo) {
        this.foodSubCategoryInfo = foodSubCategoryInfo;
    }

    public Map<String, Double> getRetailCategoryInfo() {
        return retailCategoryInfo;
    }

    public void setRetailCategoryInfo(Map<String, Double> retailCategoryInfo) {
        this.retailCategoryInfo = retailCategoryInfo;
    }

    @Override
    public String toString() {
        return isRetailInfo + " " + foodCategoryInfo + " " + foodSubCategoryInfo + " " + retailCategoryInfo;
    }
}
