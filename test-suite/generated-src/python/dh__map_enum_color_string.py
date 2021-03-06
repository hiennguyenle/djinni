# AUTOGENERATED FILE - DO NOT MODIFY!
# This file generated by Djinni from test.djinni

from djinni.support import MultiSet # default imported in all files
from djinni.exception import CPyException # default imported in all files
from djinni.pycffi_marshal import CPyEnum, CPyObject, CPyObjectProxy, CPyString

from color import Color
from PyCFFIlib_cffi import ffi, lib

from djinni import exception # this forces run of __init__.py which gives cpp option to call back into py to create exception

class MapEnumColorStringHelper:
    c_data_set = MultiSet()

    @staticmethod
    def check_c_data_set_empty():
        assert len(MapEnumColorStringHelper.c_data_set) == 0
        Color.check_c_data_set_empty()

    @ffi.callback("struct DjinniString *(struct DjinniObjectHandle *, int)")
    def __get_value(cself, key):
        pyKey = CPyEnum.toPy(Color, key)
        assert pyKey is not None
        try:
            with CPyString.fromPy(CPyObjectProxy.toPyObj(None, cself)[pyKey]) as py_obj:
                _ret = py_obj.release_djinni_string()
                assert _ret != ffi.NULL
                return _ret
        except Exception as _djinni_py_e:
            CPyException.setExceptionFromPy(_djinni_py_e)
            return ffi.NULL

    @ffi.callback("size_t(struct DjinniObjectHandle *)")
    def __get_size(cself):
        return len(CPyObjectProxy.toPyObj(None, cself))

    @ffi.callback("struct DjinniObjectHandle *()")
    def __python_create():
        c_ptr = ffi.new_handle(MapEnumColorStringProxy(dict()))
        MapEnumColorStringHelper.c_data_set.add(c_ptr)
        return ffi.cast("struct DjinniObjectHandle *", c_ptr)

    @ffi.callback("void(struct DjinniObjectHandle *, int, struct DjinniString *)")
    def __python_add(cself, key, value):
        CPyObjectProxy.toPyObj(None, cself)[CPyEnum.toPy(Color, key)] = CPyString.toPy(value)

    @ffi.callback("void(struct DjinniObjectHandle * )")
    def __delete(c_ptr):
        assert c_ptr in MapEnumColorStringHelper.c_data_set
        MapEnumColorStringHelper.c_data_set.remove(c_ptr)

    @ffi.callback("int(struct DjinniObjectHandle *)")
    def __python_next(cself):
        try:
            _ret = CPyEnum.fromPy(next(CPyObjectProxy.toPyIter(cself)))
            assert _ret != -1
            return _ret
        except Exception as _djinni_py_e:
            CPyException.setExceptionFromPy(_djinni_py_e)
            return ffi.NULL

    @staticmethod
    def _add_callbacks():
        lib.map_enum_color_string_add_callback__get_value(MapEnumColorStringHelper.__get_value)
        lib.map_enum_color_string_add_callback___delete(MapEnumColorStringHelper.__delete)
        lib.map_enum_color_string_add_callback__get_size(MapEnumColorStringHelper.__get_size)
        lib.map_enum_color_string_add_callback__python_create(MapEnumColorStringHelper.__python_create)
        lib.map_enum_color_string_add_callback__python_add(MapEnumColorStringHelper.__python_add)
        lib.map_enum_color_string_add_callback__python_next(MapEnumColorStringHelper.__python_next)

MapEnumColorStringHelper._add_callbacks()

class MapEnumColorStringProxy:
    def iter(d):
        for k in d:
            yield k

    def __init__(self, py_obj):
        self._py_obj = py_obj
        if py_obj is not None:
            self._py_iter = iter(py_obj)
        else:
            self._py_iter = None
