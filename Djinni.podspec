Pod::Spec.new do |s|
    s.name             = 'Djinni'
    s.version          = '4.6.1'
    s.summary          = 'Djinni is a tool for generating cross-language type declarations and interface bindings.'
    s.description      = <<-DESC
    Djinni is a tool for generating cross-language type declarations and interface bindings.
    It's designed to connect C++ with either Java or Objective-C. Python support is experimental. 
    For more information see README.Python.md
                           DESC
    s.homepage         = 'https://github.com/hiennguyenle/finn'
    
    s.license          = { :type => 'MIT' }
    
    s.author           = { 'Hien Nguyen' => 'hien.nguyenle.it@gmail.com' }
    
    s.source           = { :git => 'https://github.com/hiennguyenle/finn.git', :tag => "#{s.version}" }
    
    s.module_name = 'Djinni'
    s.requires_arc = true
    s.library = 'c++'
    
    s.dependency 'DjinniSwift', "#{s.version}"
    
    s.ios.deployment_target  = '12.0'
    s.osx.deployment_target  = '10.10'
        
    s.pod_target_xcconfig = {
        'HEADER_SEARCH_PATHS' => [
            'support-lib/objc'
        ],
        'USER_HEADER_SEARCH_PATHS' => [
            'support-lib/extension/json/nlohmann'
        ]
    }
    
    s.source_files         = [
        "support-lib/extension/json/nlohmann/*.hpp",
        "support-lib/extension/json/objc/DJIMarshal+Json.h",
        "support-lib/objc/*.{h,mm}",
        "support-lib/proxy_cache_impl.hpp",
        "support-lib/proxy_cache_interface.hpp"
    ]
end
