package org.search.pretreat.core.model.request;

public class PretreatRequest {

	private String query;
	private String cityId;
	private String abInfo;
	private double latitude;
	private double longitude;

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}

	public String getCityId() {
		return cityId;
	}

	public void setCityId(String cityId) {
		this.cityId = cityId;
	}

	public String getAbInfo() {
		return abInfo;
	}

	public void setAbInfo(String abInfo) {
		this.abInfo = abInfo;
	}

	public double getLatitude() {
		return latitude;
	}

	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}

	public double getLongitude() {
		return longitude;
	}

	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}

	@Override
	public String toString() {
		return "PretreatRequest [query=" + query + ", cityId=" + cityId + ", abInfo=" + abInfo + ", longitude="
				+ longitude + ", latitude=" + latitude + "]";
	}

}
