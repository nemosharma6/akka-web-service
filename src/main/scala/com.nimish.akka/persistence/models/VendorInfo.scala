package com.nimish.akka.persistence.models

case class VendorInfo(number: Int, name: String, deptNbr: Int)
case class VendorAddressRequest(vendorNumber: Int)
case class VendorAddressResponse(addressList: Seq[Option[ReturnAddress]])
