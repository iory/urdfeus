cmake_minimum_required(VERSION 3.0.2)
project(asahikasei_small_basket_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
