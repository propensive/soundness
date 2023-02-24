package anticipation

import diuretic.*

package timeApi:
  given javaTime: JavaTime.type = JavaTime
  given long: JavaLongTime.type = JavaLongTime
  given javaUtil: JavaUtilTime.type = JavaUtilTime

package fileApi:
  given javaNio: JavaNioFile.type = JavaNioFile
  given javaIo: JavaIoFile.type = JavaIoFile

package urlApi:
  given javaNet: JavaNetUrl.type = JavaNetUrl
